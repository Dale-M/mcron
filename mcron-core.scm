;;   Copyright (C) 2003 Dale Mellor
;; 
;;   This program is free software; you can redistribute it and/or modify
;;   it under the terms of the GNU General Public License as published by
;;   the Free Software Foundation; either version 2, or (at your option)
;;   any later version.
;; 
;;   This program is distributed in the hope that it will be useful,
;;   but WITHOUT ANY WARRANTY; without even the implied warranty of
;;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;   GNU General Public License for more details.
;; 
;;   You should have received a copy of the GNU General Public License
;;   along with this program; if not, write to the Free Software
;;   Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
;;   USA.



(define-module (mcron core)
  #:use-module (mcron environment)
  #:export     (add-job
                remove-user-jobs
                get-schedule
                run-job-loop
                   ;; These three are deprecated and not documented.
                use-system-job-list
                use-user-job-list
                clear-system-jobs)
  #:re-export  (clear-environment-mods
                append-environment-mods))


(use-modules (srfi srfi-1)    ;; For remove.
             (srfi srfi-2))   ;; For and-let*.



;; The list of all jobs known to the system. Each element of the list is
;;
;;  (vector user next-time-function action environment displayable next-time)
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



;; Convenience functions for getting and setting the elements of a job object.

(define (job:user job)                (vector-ref job 0))
(define (job:next-time-function job)  (vector-ref job 1))
(define (job:action job)              (vector-ref job 2))
(define (job:environment job)         (vector-ref job 3))
(define (job:displayable job)         (vector-ref job 4))
(define (job:next-time job)           (vector-ref job 5))



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
  (let ((entry (vector configuration-user
                       time-proc
                       action
                       (get-current-environment-mods-copy)
                       displayable
                       (time-proc configuration-time))))
    (if (eq? configuration-source 'user)
      (set! user-job-list (cons entry user-job-list))
      (set! system-job-list (cons entry system-job-list)))))



;; Procedure to locate the jobs in the global job-list with the lowest
;; (soonest) next-times. These are the jobs for which we must schedule the mcron
;; program (under any personality) to next wake up. The return value is a cons
;; cell consisting of the next time (maintained in the next-time variable) and a
;; list of the job entries that are to run at this time (maintained in the
;; next-jobs-list variable).
;;
;; The procedure works by first obtaining the time of the first job on the list,
;; and setting this job in the next-jobs-list. Then for each other entry on the
;; job-list, either the job runs earlier than any other that have been scanned,
;; in which case the next-time and next-jobs-list are re-initialized to
;; accomodate, or the job runs at the same time as the next job, in which case
;; the next-jobs-list is simply augmented with the new job, or else the job runs
;; later than others noted in which case we ignore it for now and continue to
;; recurse the list.

(define (find-next-jobs)
  (let ((job-list (append system-job-list user-job-list)))
    
    (if (null? job-list)
        
        '(#f . '())
        
        (let ((next-time 2000000000)
              (next-jobs-list '()))

          (for-each
           (lambda (job)
             (let ((this-time (job:next-time job)))
               (cond ((< this-time next-time)
                          (set! next-time this-time)
                          (set! next-jobs-list (list job)))
                     ((eqv? this-time next-time)
                          (set! next-jobs-list (cons job next-jobs-list))))))
           job-list)

          (cons next-time next-jobs-list)))))



;; Create a string containing a textual list of the next count jobs to run.
;;
;; Enter a loop of displaying the next set of jobs to run, artificially
;; forwarding the time to the next time point (instead of waiting for it to
;; occur as we would do in a normal run of mcron), and recurse around the loop
;; count times.
;;
;; Note that this has the effect of mutating the job timings. Thus the program
;; must exit after calling this function; the internal data state will be left
;; unusable.

(define (get-schedule count)
  (with-output-to-string
    (lambda ()
      (do ((count count (- count 1)))
          ((eqv? count 0))
        (and-let* ((next-jobs (find-next-jobs))
                   (time (car next-jobs))
                   (date-string (strftime "%c\n" (localtime time))))
          (for-each (lambda (job)
                      (display date-string)
                      (display (job:displayable job))
                      (newline)(newline)
                      (vector-set! job
                                   5
                                   ((job:next-time-function job)
                                                          (job:next-time job))))
                    (cdr next-jobs)))))))



;; For proper housekeeping, it is necessary to keep a record of the number of
;; child processes we fork off to run the jobs.

(define number-children 0)



;; For every job on the list, fork a process to run it (noting the fact by
;; increasing the number-children counter), and in the new process set up the
;; run-time environment exactly as it should be before running the job proper.
;;
;; In the parent, update the job entry by computing the next time the job needs
;; to run.

(define (run-jobs jobs-list)
  (for-each (lambda (job)
              (if (eqv? (primitive-fork) 0)
                  (begin
                    (setgid (passwd:gid (job:user job)))
                    (setuid (passwd:uid (job:user job)))
                    (chdir (passwd:dir (job:user job)))
                    (modify-environment (job:environment job) (job:user job))
                    ((job:action job))
                    (primitive-exit 0))
                  (begin
                    (set! number-children (+ number-children 1))
                    (vector-set! job
                                 5
                                 ((job:next-time-function job)
                                                            (current-time))))))
            jobs-list))



;; Now the main loop. Loop over all job specifications, get a list of the next
;; ones to run (may be more than one). Set an alarm and go to sleep. When we
;; wake, run the jobs and reap any children (old jobs) that have
;; completed. Repeat ad infinitum.
;;
;; Note that, if we wake ahead of time, it can only mean that a signal has been
;; sent by a crontab job to tell us to re-read a crontab file. In this case we
;; break out of the loop here, and let the main procedure deal with the
;; situation (it will eventually re-call this function, thus maintaining the
;; loop).

(define (run-job-loop . fd-list)

  (call-with-current-continuation (lambda (break)
  
    (let ((fd-list (if (null? fd-list) '() (car fd-list))))

      (let loop ()

        (let* ((next-jobs      (find-next-jobs))
               (next-time      (car next-jobs))
               (next-jobs-list (cdr next-jobs))
               (sleep-time     (if next-time (- next-time (current-time))
                                   2000000000)))

          (and (> sleep-time 0)
               (if (not (null? (car (select fd-list '() '() sleep-time))))
                   (break)))
        
          (run-jobs next-jobs-list)

          (do () ((or (<= number-children 0)
                      (eqv? (car (waitpid WAIT_ANY WNOHANG)) 0)))
            (set! number-children (- number-children 1)))
        
          (loop)))))))
