;;   Copyright (C) 2003 Dale Mellor
;;   Copyright (C) 2016 Mathieu Lirzin
;; 
;;   This file is part of GNU mcron.
;;
;;   GNU mcron is free software: you can redistribute it and/or modify it under
;;   the terms of the GNU General Public License as published by the Free
;;   Software Foundation, either version 3 of the License, or (at your option)
;;   any later version.
;;
;;   GNU mcron is distributed in the hope that it will be useful, but WITHOUT
;;   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;;   FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;;   more details.
;;
;;   You should have received a copy of the GNU General Public License along
;;   with GNU mcron.  If not, see <http://www.gnu.org/licenses/>.



;; This module defines all the functions that can be used by scheme mcron
;; configuration files, namely the procedures for working out next times, the
;; job procedure for registering new jobs (actually a wrapper around the core
;; add-job function), and the procedure for declaring environment modifications.

(define-module (mcron job-specifier)
  #:export (range
            next-year-from         next-year
            next-month-from        next-month
            next-day-from          next-day
            next-hour-from         next-hour
            next-minute-from       next-minute
            next-second-from       next-second
            set-configuration-user
            set-configuration-time
            job
            find-best-next)
  #:use-module (mcron core)
  #:use-module (mcron environment)
  #:use-module (mcron vixie-time)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:re-export (append-environment-mods))


(define* (range start end #:optional (step 1))
  "Produces a list of values from START up to (but not including) END.  An
optional STEP may be supplied, and (if positive) only every step'th value will
go into the list.  For example, (range 1 6 2) returns '(1 3 5)."
  (unfold (cut >= <> end) identity (cute + <> (max step 1)) start))

;; Internal function (not supposed to be used directly in configuration files;
;; it is exported from the module for the convenience of other parts of the
;; mcron implementation) which takes a value and a list of possible next values
;; (all assumed less than 9999). It returns a pair consisting of the smallest
;; element of the list, and the smallest element larger than the current
;; value. If an example of the latter cannot be found, 9999 will be returned.

(define (find-best-next current next-list)
  (let ((current-best (cons 9999 9999)))
    (for-each (lambda (allowed-time)
                          (if (< allowed-time (car current-best))
                              (set-car! current-best allowed-time))
                          (if (and (> allowed-time current)
                                   (< allowed-time (cdr current-best)))
                              (set-cdr! current-best allowed-time)))
              next-list)
    current-best))



;; Internal function to return the time corresponding to some near future
;; hour. If hour-list is not supplied, the time returned corresponds to the
;; start of the next hour of the day.
;;
;; If the hour-list is supplied the time returned corresponds to the first hour
;; of the day in the future which is contained in the list. If all the values in
;; the list are less than the current hour, then the time returned will
;; correspond to the first hour in the list *on the following day*.
;;
;; ... except that the function is actually generalized to deal with seconds,
;; minutes, etc., in an obvious way :-)
;;
;; Note that value-list always comes from an optional argument to a procedure,
;; so is wrapped up as the first element of a list (i.e. it is a list inside a
;; list).

(define (bump-time time value-list component higher-component
                   set-component! set-higher-component!)
  (if (null? value-list)
      (set-component! time (+ (component time) 1))
      (let ((best-next (find-best-next (component time) (car value-list))))
        (if (eqv? 9999 (cdr best-next))
            (begin
              (set-higher-component! time (+ (higher-component time) 1))
              (set-component! time (car best-next)))
            (set-component! time (cdr best-next)))))
  (car (mktime time)))




;; Set of configuration methods which use the above general function to bump
;; specific components of time to the next legitimate value. In each case, all
;; the components smaller than that of interest are taken to zero, so that for
;; example the time of the next year will be the time at which the next year
;; actually starts.

(define (next-year-from current-time . year-list)
  (let ((time (localtime current-time)))
    (set-tm:mon   time 0)
    (set-tm:mday  time 1)
    (set-tm:hour  time 0)
    (set-tm:min   time 0)
    (set-tm:sec   time 0)
    (bump-time time year-list tm:year tm:year set-tm:year set-tm:year)))

(define (next-month-from current-time . month-list)
  (let ((time (localtime current-time)))
    (set-tm:mday  time 1)
    (set-tm:hour  time 0)
    (set-tm:min   time 0)
    (set-tm:sec   time 0)
    (bump-time time month-list tm:mon tm:year set-tm:mon set-tm:year)))

(define (next-day-from current-time . day-list)
  (let ((time (localtime current-time)))
    (set-tm:hour  time 0)
    (set-tm:min   time 0)
    (set-tm:sec   time 0)
    (bump-time time day-list tm:mday tm:mon set-tm:mday set-tm:mon)))

(define (next-hour-from current-time . hour-list)
  (let ((time (localtime current-time)))
    (set-tm:min   time 0)
    (set-tm:sec   time 0)
    (bump-time time hour-list tm:hour tm:mday set-tm:hour set-tm:mday)))

(define (next-minute-from current-time . minute-list)
  (let ((time (localtime current-time)))
    (set-tm:sec   time 0)
    (bump-time time minute-list tm:min tm:hour set-tm:min set-tm:hour)))

(define (next-second-from current-time . second-list)
  (let ((time (localtime current-time)))
    (bump-time time second-list tm:sec tm:min set-tm:sec set-tm:min)))



;; The current-action-time is the time a job was last run, the time from which
;; the next time to run a job must be computed. (When the program is first run,
;; this time is set to the configuration time so that jobs run from that moment
;; forwards.) Once we have this, we supply versions of the time computation
;; commands above which implicitly assume this value.

(define current-action-time 0)



;; We want to provide functions which take a single optional argument (as well
;; as implicitly the current action time), but unlike usual scheme behaviour if
;; the argument is missing we want to act like it is really missing, and if it
;; is there we want to act like it is a genuine argument, not a list of
;; optionals.

(define (maybe-args function args)
  (if (null? args)
      (function current-action-time)
      (function current-action-time (car args))))



;; These are the convenience functions we were striving to define for the
;; configuration files. They are wrappers for the next-X-from functions above,
;; but implicitly use the current-action-time for the time argument.

(define (next-year   . args) (maybe-args next-year-from args))
(define (next-month  . args) (maybe-args next-month-from args))
(define (next-day    . args) (maybe-args next-day-from args))
(define (next-hour   . args) (maybe-args next-hour-from args))
(define (next-minute . args) (maybe-args next-minute-from args))
(define (next-second . args) (maybe-args next-second-from args))



;; The default user for running jobs is the current one (who invoked this
;; program). There are exceptions: when cron parses /etc/crontab the user is
;; specified on each individual line; when cron parses /var/cron/tabs/* the user
;; is derived from the filename of the crontab. These cases are dealt with by
;; mutating this variable. Note that the variable is only used at configuration
;; time; a UID is stored with each job and it is that which takes effect when
;; the job actually runs.

(define configuration-user (getpw (getuid)))
(define configuration-time (current-time))

(define (set-configuration-user user)
  (set! configuration-user (if (or (string? user)
                                   (integer? user))
                               (getpw user)
                               user)))
(define (set-configuration-time time) (set! configuration-time time))



;; The job function, available to configuration files for adding a job rule to
;; the system.
;;
;; Here we must 'normalize' the next-time-function so that it is always a lambda
;; function which takes one argument (the last time the job ran) and returns a
;; single value (the next time the job should run). If the input value is a
;; string this is parsed as a Vixie-style time specification, and if it is a
;; list then we arrange to eval it (but note that such lists are expected to
;; ignore the function parameter - the last run time is always read from the
;; current-action-time global variable). A similar normalization is applied to
;; the action.
;;
;; Here we also compute the first time that the job is supposed to run, by
;; finding the next legitimate time from the current configuration time (set
;; right at the top of this program).

(define (job time-proc action . displayable)
  (let ((action (cond ((procedure? action) action)
                      ((list? action) (lambda () (primitive-eval action)))
                      ((string? action) (lambda () (system action)))
                      (else 
           (throw 'mcron-error 2
                  "job: invalid second argument (action; should be lambda "
                  "function, string or list)"))))

        (time-proc
         (cond ((procedure? time-proc) time-proc)
               ((string? time-proc)    (parse-vixie-time time-proc))
               ((list? time-proc)      (lambda (current-time)
                                         (primitive-eval time-proc)))
               (else
                (throw 'mcron-error 3
                       "job: invalid first argument (next-time-function; "
                       "should be function, string or list)"))))
        (displayable
         (cond ((not (null? displayable)) (car displayable))
               ((procedure? action) "Lambda function")
               ((string? action) action)
               ((list? action) (with-output-to-string
                                 (lambda () (display action)))))))
    (add-job (lambda (current-time)
               (set! current-action-time current-time)  ;; ?? !!!!  Code
               
               ;; Contributed by Sergey Poznyakoff to allow for daylight savings
               ;; time changes.
               (let* ((next (time-proc current-time))
                      (gmtoff (tm:gmtoff (localtime next)))
                      (d (+ next (- gmtoff
                                    (tm:gmtoff (localtime current-time))))))
                 (if (eqv? (tm:gmtoff (localtime d)) gmtoff)
                     d
                     next)))
             action
             displayable
             configuration-time
             configuration-user)))
