;;;; base.scm -- core procedures
;;; Copyright © 2003 Dale Mellor <dale_mellor@users.sourceforge.net>
;;; Copyright © 2015, 2016, 2018 Mathieu Lirzin <mthl@gnu.org>
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

;;;; Commentary:
;;;
;;; This module provides the core data structures for scheduling jobs and the
;;; procedures for running those jobs.
;;;
;;;; Code:

(define-module (mcron base)
  #:use-module (ice-9 match)
  #:use-module (mcron environment)
  #:use-module (mcron utils)
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

;; A cron job.
(define-record-type <job>
  (make-job user time-proc action environment displayable next-time)
  job?
  (user        job:user)                ;object : passwd entry
  (time-proc   job:next-time-function)  ;proc   : with one 'time' parameter
  (action      job:action)              ;thunk  : user's code
  ;; Environment variables that need to be set before the ACTION is run.
  (environment job:environment)         ;alist  : environment variables
  (displayable job:displayable)         ;string : visible in schedule
  (next-time   job:next-time            ;number : time in UNIX format
               job:next-time-set!))

;; A schedule of cron jobs.
(define-record-type <schedule>
  ;; The schedule is composed of a 'user' and 'system' schedule.  This makes
  ;; removing all the jobs belonging to one group easy, which is required for
  ;; full vixie compatibility.
  (make-schedule user system current)
  schedule?
  ;; list for jobs that may be placed in '/etc/crontab'.
  (system  schedule-system  set-schedule-system!)   ;list of <job>
  ;; list for all other jobs.
  (user    schedule-user    set-schedule-user!)     ;list of <job>
  (current schedule-current set-schedule-current!)) ;symbol 'user or 'system

(define %global-schedule
  ;; Global schedule used by 'mcron' and 'cron'.
  (make-schedule '() '() 'user))

(define* (use-system-job-list #:key (schedule %global-schedule))
  "Mutate '%global-schedule' to use system jobs.
This procedure is deprecated."
  (set-schedule-current! schedule 'system))

(define* (use-user-job-list #:key (schedule %global-schedule))
  "Mutate '%global-schedule' to use user jobs.
This procedure is deprecated."
  (set-schedule-current! schedule 'user))

(define* (remove-user-jobs user #:key (schedule %global-schedule))
  "Remove user jobs from SCHEDULE belonging to USER.  USER must be either a
username, a UID, or a passwd entry."
  (let ((user* (get-user user)))
    (set-schedule-user! schedule
                        (filter (lambda (job)
                                  (not (eqv? (passwd:uid user*)
                                             (passwd:uid (job:user job)))))
                                (schedule-user schedule)))))

(define* (clear-system-jobs #:key (schedule %global-schedule))
  "Remove all the system jobs from SCHEDULE."
  (set-schedule-system! schedule '()))

(define* (add-job time-proc action displayable configuration-time
                 configuration-user
                 #:key (schedule %global-schedule))
  "Add a new job with the given specifications to the current job set in
SCHEDULE."
  (let ((entry (make-job configuration-user
                         time-proc
                         action
                         (get-current-environment-mods-copy)
                         displayable
                         (time-proc configuration-time))))
    (if (eq? (schedule-current schedule) 'user)
        (set-schedule-user! schedule (cons entry (schedule-user schedule)))
        (set-schedule-system! schedule
                              (cons entry (schedule-system schedule))))))

(define* (find-next-jobs #:key (schedule %global-schedule))
  "Locate the jobs in SCHEDULE with the lowest (soonest) next-times.  Return a
list where the head is the next scheduled time and the rest are all the job
entries that are to run at this time.  When SCHEDULE is empty next time is
'#f'."
  (let loop ((jobs
              (append (schedule-system schedule) (schedule-user schedule)))
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

(define* (display-schedule count #:optional (port (current-output-port))
                           #:key (schedule %global-schedule))
  "Display on PORT a textual list of the next COUNT jobs to run.  This
simulates the run of the job loop to display the resquested information.
Since calling this procedure has the effect of mutating the job timings, the
program must exit after.  Otherwise the internal data state will be left
unusable."
  (unless (<= count 0)
    (match (find-next-jobs #:schedule schedule)
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
    (display-schedule (- count 1) port #:schedule schedule)))

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

(define (child-cleanup)
  ;; Give any zombie children a chance to die, and decrease the number known
  ;; to exist.
  (unless (or (<= number-children 0)
              (= (car (waitpid WAIT_ANY WNOHANG)) 0))
    (set! number-children (- number-children 1))
    (child-cleanup)))

(define* (run-job-loop #:optional fd-list #:key (schedule %global-schedule))
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
       (match (find-next-jobs #:schedule schedule)
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
