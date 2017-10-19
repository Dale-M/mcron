;;;; base.scm -- core procedures
;;; Copyright © 2003 Dale Mellor <dale_mellor@users.sourceforge.net>
;;; Copyright © 2015, 2016 Mathieu Lirzin <mthl@gnu.org>
;;; Copyright © 2016 Ludovic Courtès <ludo@gnu.org>
;;;
;;; This file is part of GNU Mcron.
;;;
;;; GNU Mcron is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; GNU Mcron is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Mcron.  If not, see <http://www.gnu.org/licenses/>.

(define-module (mcron base)
  #:use-module (ice-9 match)
  #:use-module (mcron environment)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-9)
  #:export (add-job
            remove-user-jobs
            display-schedule
            run-job-loop
            ;; Deprecated and undocumented procedures.
            use-system-job-list
            use-user-job-list
            clear-system-jobs)
  #:re-export (clear-environment-mods
               append-environment-mods))

;; The list of all jobs known to the system. Each element of the list is
;;
;;  (make-job user next-time-function action environment displayable next-time)
;;
;; where action must be a procedure, and the environment is an alist of
;; modifications that need making to the UNIX environment before the action is
;; run. The next-time element is the only one that is modified during the
;; running of a cron process (i.e. all the others are set once and for all at
;; configuration time).
;;
;; The reason we maintain two lists is that jobs in /etc/crontab may be placed
;; in one, and all other jobs go in the other. This makes it possible to remove
;; all the jobs in the first list in one go, and separately we can remove all
;; jobs from the second list which belong to a particular user. This behaviour
;; is required for full vixie compatibility.

(define system-job-list '())
(define user-job-list '())

(define configuration-source 'user)

(define (use-system-job-list) (set! configuration-source 'system))
(define (use-user-job-list) (set! configuration-source 'user))

;; A cron job.
(define-record-type <job>
  (make-job user time-proc action environment displayable next-time)
  job?
  (user        job:user)                ;object : passwd entry
  (time-proc   job:next-time-function)  ;proc   : with one 'time' parameter
  (action      job:action)              ;thunk  : user's code
  (environment job:environment)         ;alist  : environment variables
  (displayable job:displayable)         ;string : visible in schedule
  (next-time   job:next-time            ;number : time in UNIX format
               job:next-time-set!))

;; Remove jobs from the user-job-list belonging to this user.

(define (remove-user-jobs user)
  (if (or (string? user)
          (integer? user))
      (set! user (getpw user)))
    (set! user-job-list
          (remove (lambda (job) (eqv? (passwd:uid user)
                                      (passwd:uid (job:user job))))
                  user-job-list)))



;; Remove all the jobs on the system job list.

(define (clear-system-jobs) (set! system-job-list '()))



;; Add a new job with the given specifications to the head of the appropriate
;; jobs list.

(define (add-job time-proc action displayable configuration-time
                 configuration-user)
  (let ((entry (make-job configuration-user
                         time-proc
                         action
                         (get-current-environment-mods-copy)
                         displayable
                         (time-proc configuration-time))))
    (if (eq? configuration-source 'user)
      (set! user-job-list (cons entry user-job-list))
      (set! system-job-list (cons entry system-job-list)))))

(define (find-next-jobs)
  "Procedure to locate the jobs in the global job-list with the
lowest (soonest) next-times.  These are the jobs for which we must schedule
the mcron program (under any personality) to next wake up.  The return value
is a cons cell consisting of the next time (maintained in the next-time
variable) and a list of the job entries that are to run at this
time (maintained in the next-jobs-list variable).

The procedure works by first obtaining the time of the first job on the list,
and setting this job in the next-jobs-list.  Then for each other entry on the
job-list, either the job runs earlier than any other that have been scanned,
in which case the next-time and next-jobs-list are re-initialized to
accomodate, or the job runs at the same time as the next job, in which case
the next-jobs-list is simply augmented with the new job, or else the job runs
later than others noted in which case we ignore it for now and continue to
recurse the list."
  (let loop ((jobs      (append system-job-list user-job-list))
             (next-time (inf))
             (next-jobs '()))
    (match jobs
      (()
       (cons (and (finite? next-time) next-time) next-jobs))
      ((job . rest)
       (let ((this-time (job:next-time job)))
         (cond ((< this-time next-time)
                (loop rest this-time (list job)))
               ((= this-time next-time)
                (loop rest next-time (cons job next-jobs)))
               (else
                (loop rest next-time next-jobs))))))))

(define* (display-schedule count #:optional (port (current-output-port)))
  "Display on PORT a textual list of the next COUNT jobs to run.  This
simulates the run of the job loop to display the resquested information.
Since calling this procedure has the effect of mutating the job timings, the
program must exit after.  Otherwise the internal data state will be left
unusable."
  (unless (<= count 0)
    (match (find-next-jobs)
      ((#f . jobs)
       #f)
      ((time . jobs)
       (let ((date-string (strftime "%c %z\n" (localtime time))))
         (for-each (lambda (job)
                     (display date-string port)
                     (display (job:displayable job) port)
                     (newline port)
                     (newline port)
                     (job:next-time-set! job ((job:next-time-function job)
                                              (job:next-time job))))
                   jobs))))
    (display-schedule (- count 1) port)))

;; For proper housekeeping, it is necessary to keep a record of the number of
;; child processes we fork off to run the jobs.

(define number-children 0)

(define (run-job job)
  "Run JOB in a separate process. The process is run as JOB user with the
environment properly set.  Update the NEXT-TIME field of JOB by computing its
next value."
  (if (= (primitive-fork) 0)
      (dynamic-wind                     ;child
        (const #t)
        (λ ()
          (setgid (passwd:gid (job:user job)))
          (setuid (passwd:uid (job:user job)))
          (chdir (passwd:dir (job:user job)))
          (modify-environment (job:environment job) (job:user job))
          ((job:action job)))
        (λ ()
          (primitive-exit 0)))
      (begin                            ;parent
        (set! number-children (+ number-children 1))
        (job:next-time-set! job ((job:next-time-function job)
                                 (current-time))))))

;; Give any zombie children a chance to die, and decrease the number known to
;; exist.

(define (child-cleanup)
  (do () ((or (<= number-children 0)
	      (eqv? (car (waitpid WAIT_ANY WNOHANG)) 0)))
    (set! number-children (- number-children 1))))

(define* (run-job-loop #:optional fd-list)
  ;; Loop over all job specifications, get a list of the next ones to run (may
  ;; be more than one).  Set an alarm and go to sleep.  When we wake, run the
  ;; jobs and reap any children (old jobs) that have completed. Repeat ad
  ;; infinitum.
  ;;
  ;; Note that, if we wake ahead of time, it can only mean that a signal has
  ;; been sent by a crontab job to tell us to re-read a crontab file.  In this
  ;; case we break out of the loop here, and let the main procedure deal with
  ;; the situation (it will eventually re-call this function, thus maintaining
  ;; the loop).
  (call-with-current-continuation
   (lambda (break)
     (let loop ()
       (match (find-next-jobs)
         ((next-time . next-jobs-lst)
          (let ((sleep-time (if next-time
                                (- next-time (current-time))
                                2000000000)))
            (when (and
                   (> sleep-time 0)
                   (not (null? (catch 'system-error
                                 (λ ()
                                   (car (select fd-list '() '() sleep-time)))
                                 (λ (key . args)
                                   (let ((err (car (last args))))
                                     (cond ((member err (list EINTR EAGAIN))
                                            (child-cleanup)
                                            '())
                                           (else
                                            (apply throw key args)))))))))
              (break))
            (for-each run-job next-jobs-lst)
            (child-cleanup)
            (loop))))))))
