;;;; base.scm -- core procedures
;;; Copyright © 2003 Dale Mellor <dale_mellor@users.sourceforge.net>
;;; Copyright © 2015, 2016, 2018 Mathieu Lirzin <mthl@gnu.org>
;;; Copyright © 2016, 2020 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2021 Maxim Cournoyer <maxim.cournoyer@gmail.com>
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
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 control)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 suspendable-ports)
  #:use-module (mcron environment)
  #:use-module (mcron utils)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-9)
  #:use-module ((srfi srfi-19) #:prefix srfi-19:)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-111)
  #:export (add-job
            remove-user-jobs
            display-schedule
            run-job-loop

            %date-format
            %log-format
            validate-date-format
            validate-log-format

            ;; Deprecated and undocumented procedures.
            use-system-job-list
            use-user-job-list
            clear-system-jobs)
  #:re-export (clear-environment-mods
               append-environment-mods))

(install-suspendable-ports!)

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

;; A (srfi srfi-19) format string for the date.  It is used to format the
;; timestamp argument.  Defaults to the local ISO-8601 date/time format.
(define %date-format (make-parameter "~5"))

(define (validate-date-format fmt)
  "Validate that FMT is a valid srfi-19 date format string."
  (let ((time (srfi-19:current-time)))
    (unless (false-if-exception
             (srfi-19:date->string (srfi-19:time-utc->date time) fmt))
      (throw 'mcron-error 1 "invalid srfi-19 date format string
hint: consult 'info \"(guile) SRFI-19 Date to string\"'"))))

;; An (ice-9 format) format string.  The positional arguments applied to
;; format are:
;; 1. the timestamp;
;; 2. the job process PID;
;; 3. the action name;
;; 4. the message.
(define %log-format (make-parameter "~a ~2@*~a: ~a~%"))

(define (validate-log-format fmt)
  "Validate that FMT is a valid (ice-9 format) log format string."
  (unless (with-output-to-port (%make-void-port "w")
            (lambda ()
              (with-error-to-port (%make-void-port "w")
                (lambda ()
                  (false-if-exception
                   (format #f "~@?" fmt
                           "2021-08-17T15:23:12" 39143 "dummy" "message"))))))
    (throw 'mcron-error 1 "invalid (ice-9 format) format string
hint: consult 'info \"(guile) Formatted Output\"'")))

;; Data about a running job process.
(define-record-type <job-data>
  (make-job-data pid port continuation name)
  job-data?
  (pid          job-data:pid)           ;string : the job process PID
  (port         job-data:port)          ;port   : an input
  (continuation job-data:continuation   ;a partial continuation to read port
                set-job-data-continuation!)
  (name         job-data:name))         ;string : the name of the job action

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
simulates the run of the job loop to display the requested information.
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

;;;
;;; Running jobs
;;;

(define number-children
  ;; For proper housekeeping, it is necessary to keep a record of the number
  ;; of child processes we fork off to run the jobs.
  (box 0))

(define (update-number-children! proc)
  ;; Apply PROC to the value stored in 'number-children'.
  (set-box! number-children (proc (unbox number-children))))

(define* (run-job job)
  "Run JOB in a separate process.  The process is run as JOB user with the
environment properly set.  Update the NEXT-TIME field of JOB by computing its
next value.  Return a <job-data> record object containing the job process
PID, the input pipe from which the process standard output and standard error
streams can be read as well as the name of the job."
  (define start (srfi-19:current-time srfi-19:time-monotonic)) ;start time

  (define (seconds-since start)
    ;; Return the time elapsed in seconds since START.
    (let* ((end (srfi-19:current-time srfi-19:time-monotonic))
           (elapsed (srfi-19:time-difference end start)))
      (+ (srfi-19:time-second elapsed)
         (* 1e-9 (srfi-19:time-nanosecond elapsed)))))

  ;; Create a pipe, and set its read side to non-blocking mode.
  (define child->parent-pipe (pipe))
  (let ((flags (fcntl (car child->parent-pipe) F_GETFL)))
    (fcntl (car child->parent-pipe) F_SETFL (logior O_NONBLOCK flags)))

  ;; Empty buffers to avoid duplicated output.
  (flush-all-ports)

  (match (primitive-fork)
    (0                                  ;child
     ;; Prepare the environment.

     ;; Connect the stdout and stderr outputs of the child process to the
     ;; pipe established in the parent.
     (close (car child->parent-pipe))   ;unused input pipe
     (dup2 (port->fdes (cdr child->parent-pipe)) 1)
     (dup2 1 2)
     (set-current-output-port (cdr child->parent-pipe))
     (set-current-error-port (cdr child->parent-pipe))

     ;; Use line buffering so the output is printed in "real time".
     (setvbuf (current-output-port) 'line)
     (setvbuf (current-error-port) 'line)

     (setgid (passwd:gid (job:user job)))
     (setuid (passwd:uid (job:user job)))
     ;; Handle a nonexistent home directory, as can be the case when running
     ;; the job as the "nobody" user.
     (catch 'system-error
       (lambda ()
         (chdir (passwd:dir (job:user job))))
       (lambda args
         (let ((errno (system-error-errno args)))
           (cond
            ((= ENOENT errno) (chdir "/"))
            (else (throw 'system-error args))))))
     (modify-environment (job:environment job) (job:user job))

     ;; Execute the action.
     (catch #t
       (lambda ()
         (format #t "running...~%")
         (flush-all-ports)
         (let* ((result ((job:action job)))
                (exit-val/maybe (false-if-exception
                                 (status:exit-val result))))
           (when (and exit-val/maybe
                      (not (= 0 exit-val/maybe)))
             (error "unclean exit status" exit-val/maybe)))
         (format #t "completed in ~,3fs~%" (seconds-since start))
         (flush-all-ports)
         (primitive-exit 0))
       (lambda args
         (format (current-error-port) "failed after ~,3fs with: ~a~%"
                 (seconds-since start) args)
         (flush-all-ports)
         (primitive-exit 1))))
    (child-pid                          ;parent
     (update-number-children! 1+)
     (job:next-time-set! job ((job:next-time-function job)
                              (current-time)))
     (close (cdr child->parent-pipe))   ;disconnect the write end of the pipe
     (make-job-data
      child-pid                         ;pid
      (car child->parent-pipe)          ;port
      #f                                ;continuation for a suspended port
      (job:displayable job)))))         ;name

(define* (process-output children-data)
  "Read the child processes output from their input port recorded in
CHILDREN-DATA and print an annotated version of each line to the standard
output port.  Ports are closed upon reading the EOF character.  As a side
effect, save the partial continuation of any suspended port to their
associated <job-data> instance."
  (define timestamp (srfi-19:date->string
                     (srfi-19:time-monotonic->date
                      (srfi-19:current-time srfi-19:time-monotonic))
                     (%date-format)))

  ;; Use line buffering so the output is printed in "real time".
  (setvbuf (current-output-port) 'line)

  (define (log-data data)
    ;; Print the lines as they become available.  Do not block when a line
    ;; could not be read.
    (let ((name (job-data:name data))
          (pid  (job-data:pid  data))
          (port (job-data:port data)))

      (define (read-line*)
        ;; Return, as a pair, the line and the terminated delimiter or end-of-file
        ;; object.  When a line cannot be read, return the '(suspended
        ;; . partial-continuation) pair, where partial-continuation can be
        ;; evaluated in the future when the port is ready to be read.
        (call-with-prompt 'continue
          (lambda ()
            (parameterize ((current-read-waiter
                            (lambda (_)
                              (abort-to-prompt 'continue))))
              (let ((cont (job-data:continuation data)))
                (if cont
                    (begin
                      (set-job-data-continuation! data #f) ;reset continuation
                      (cont))
                    ;; Also use the carriage return as a line delimiter to
                    ;; preserve the full output in a readable way.
                    (read-delimited "\n\r" port 'split)))))
          (lambda (partial-continuation)
            (cons 'suspended partial-continuation))))

      (define (format-line line)
        (format #t "~@?" (%log-format) timestamp pid name line))

      (let loop ((line+delim (read-line*)))
        (match line+delim
          (('suspended . partial-continuation)
           (set-job-data-continuation! data partial-continuation))
          ((line . (? eof-object?))
           (close port)
           (unless (eof-object? line)
             (format-line line)))
          (("" . #\cr)
           ;; A carriage return directly followed a delimiter.  Ignore it.
           (loop (read-line*)))
          ((line . _)
           (format-line line)
           (loop (read-line*)))))))

  (for-each log-data
            (remove (compose port-closed? job-data:port)
                    children-data)))

(define (child-cleanup children-data)
  "Give any zombie children a chance to die, and decrease the number known to
exist.  CHILDREN-DATA is a list of <job-data> objects.  Return the pruned list
of CHILDREN-DATA."
  (define has-children? (> (unbox number-children) 0))
  (define collected-pid (or (and has-children?
                                 (car (waitpid WAIT_ANY WNOHANG)))
                            0))
  (define (match-collected-pid? job-data)
    (= (job-data:pid job-data) collected-pid))

  (if (and has-children?
           (not (= 0 collected-pid)))   ;at least one process was collected
      (begin
        (update-number-children! 1-)
        ;; Fully collect the output of the reaped child process.
        (and=> (find match-collected-pid? children-data)
               (lambda (child-data)
                 (process-output (list child-data))))
        (child-cleanup (remove match-collected-pid? children-data)))
      children-data))

(define* (run-job-loop #:optional (fd-list '())
                       #:key (schedule %global-schedule))
  "Loop over all job specifications in SCHEDULE, get a list of the next ones
to run (may be more than one).  Set an alarm and go to sleep.  When we wake,
run the jobs and reap any children (old jobs) that have completed.  Repeat ad
infinitum."
  ;; Validate the format parameters, so that we can fail early.
  (validate-date-format (%date-format))
  (validate-log-format (%log-format))

  ;; Note that, if we wake ahead of time, it can only mean that a signal has
  ;; been sent by a crontab job to tell us to re-read a crontab file.  In this
  ;; case we break out of the loop here, and let the main procedure deal with
  ;; the situation (it will eventually re-call this function, thus maintaining
  ;; the loop).
  (cond-expand
    ((or guile-3.0 guile-2.2)                     ;2.2 and 3.0
     (define select* select))
    (else
     ;; On Guile 2.0, 'select' could throw upon EINTR or EAGAIN.
     (define (select* read write except time)
       (catch 'system-error
         (lambda ()
           (select read write except time))
         (lambda args
           (if (member (system-error-errno args) (list EAGAIN EINTR))
               '(() () ())
               (apply throw args)))))))

  (let/ec break
    (let loop ((children-data '()))     ;list of <job-data> objects
      (match (find-next-jobs #:schedule schedule)
        ((next-time . next-jobs-lst)
         (let* ((sleep-time (if next-time
                                (- next-time (current-time))
                                2000000000))
                (ports (map job-data:port children-data))
                ;; Ensure closed ports are not put back into select, otherwise
                ;; it would not block and EOFs would be read infinitely.
                (children-fdes (filter-map (lambda (p)
                                             (and (not (port-closed? p))
                                                  (port->fdes p)))
                                           ports)))
           (when (> sleep-time 0)
             (match (select* (append fd-list children-fdes)
                             '() '() sleep-time)
               ((() () ())
                ;; 'select' returned an empty set, perhaps because it got
                ;; EINTR or EAGAIN.
                (loop (child-cleanup children-data)))
               (((fdes ...) () ())
                ;; Process any children input fdes ready to be read.
                (let ((children-fdes/read-ready (lset-intersection
                                                 = children-fdes fdes)))
                  (when (not (null? (lset-difference
                                     = fdes children-fdes/read-ready)))
                    ;; There was some crontab activity so leave the loop to
                    ;; process the update request.
                    (break))
                  (unless (null? children-fdes/read-ready)
                    (process-output
                     (filter (lambda (x)
                               (member (port->fdes (job-data:port x))
                                       children-fdes/read-ready))
                             children-data))
                    (loop (child-cleanup children-data)))))))

           ;; The timeout has elapsed.  Run the scheduled job(s).
           (let ((new-children-data (map run-job next-jobs-lst))
                 (pruned-children-data (child-cleanup children-data)))
             (loop (append new-children-data pruned-children-data)))))))))
