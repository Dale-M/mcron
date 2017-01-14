;;;; job-specifier.scm -- public interface for defining jobs
;;; Copyright © 2003 Dale Mellor <dale_mellor@users.sourceforge.net>
;;; Copyright © 2016, 2017 Mathieu Lirzin <mthl@gnu.org>
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
;;; Define all the functions that can be used by scheme Mcron configuration
;;; files, namely the procedures for working out next times, the job procedure
;;; for registering new jobs (actually a wrapper around the base add-job
;;; function), and the procedure for declaring environment modifications.
;;;
;;;; Code:

(define-module (mcron job-specifier)
  #:use-module (ice-9 match)
  #:use-module (mcron base)
  #:use-module (mcron environment)
  #:use-module (mcron vixie-time)
  #:use-module (srfi srfi-1)
  #:re-export (append-environment-mods)
  #:export (range
            next-year-from         next-year
            next-month-from        next-month
            next-day-from          next-day
            next-hour-from         next-hour
            next-minute-from       next-minute
            next-second-from       next-second
            set-configuration-user
            set-configuration-time
            job))

(define* (range start end #:optional (step 1))
  "Produces a list of values from START up to (but not including) END.  An
optional STEP may be supplied, and (if positive) only every step'th value will
go into the list.  For example, (range 1 6 2) returns '(1 3 5)."
  (let ((step* (max step 1)))
    (unfold (λ (i) (>= i end))          ;predicate
            identity                    ;value
            (λ (i) (+ step* i))         ;next seed
            start)))                    ;seed

(define (%find-best-next current next-list)
  ;; Takes a value and a list of possible next values.  It returns a pair
  ;; consisting of the smallest element of the NEXT-LIST, and the smallest
  ;; element larger than the CURRENT value.  If an example of the latter
  ;; cannot be found, +INF.0 will be returned.
  (let loop ((smallest (inf)) (closest+ (inf)) (lst next-list))
    (match lst
      (() (cons smallest closest+))
      ((time . rest)
       (loop (min time smallest)
             (if (> time current) (min time closest+) closest+)
             rest)))))

(define (bump-time time value-list component higher-component
                   set-component! set-higher-component!)
  ;; Return the time corresponding to some near future hour.  If hour-list is
  ;; not supplied, the time returned corresponds to the start of the next hour
  ;; of the day.
  ;;
  ;; If the hour-list is supplied the time returned corresponds to the first
  ;; hour of the day in the future which is contained in the list.  If all the
  ;; values in the list are less than the current hour, then the time returned
  ;; will correspond to the first hour in the list *on the following day*.
  ;;
  ;; ... except that the function is actually generalized to deal with
  ;; seconds, minutes, etc., in an obvious way :-)
  ;;
  ;; Note that value-list always comes from an optional argument to a
  ;; procedure, so is wrapped up as the first element of a list (i.e. it is a
  ;; list inside a list).
  (match value-list
    (()
     (set-component! time (1+ (component time))))
    ((val . rest)
     (match (%find-best-next (component time) val)
       ((smallest . closest+)
        (cond ((inf? closest+)
               (set-higher-component! time (1+ (higher-component time)))
               (set-component! time smallest))
              (else
               (set-component! time closest+)))))))
  (first (mktime time)))

;; Set of configuration methods which use the above general function to bump
;; specific components of time to the next legitimate value. In each case, all
;; the components smaller than that of interest are taken to zero, so that for
;; example the time of the next year will be the time at which the next year
;; actually starts.

(define* (next-year-from current-time #:optional (year-list '()))
  (let ((time (localtime current-time)))
    (set-tm:mon   time 0)
    (set-tm:mday  time 1)
    (set-tm:hour  time 0)
    (set-tm:min   time 0)
    (set-tm:sec   time 0)
    (bump-time time year-list tm:year tm:year set-tm:year set-tm:year)))

(define* (next-month-from current-time #:optional (month-list '()))
  (let ((time (localtime current-time)))
    (set-tm:mday  time 1)
    (set-tm:hour  time 0)
    (set-tm:min   time 0)
    (set-tm:sec   time 0)
    (bump-time time month-list tm:mon tm:year set-tm:mon set-tm:year)))

(define* (next-day-from current-time #:optional (day-list '()))
  (let ((time (localtime current-time)))
    (set-tm:hour  time 0)
    (set-tm:min   time 0)
    (set-tm:sec   time 0)
    (bump-time time day-list tm:mday tm:mon set-tm:mday set-tm:mon)))

(define* (next-hour-from current-time #:optional (hour-list '()))
  (let ((time (localtime current-time)))
    (set-tm:min   time 0)
    (set-tm:sec   time 0)
    (bump-time time hour-list tm:hour tm:mday set-tm:hour set-tm:mday)))

(define* (next-minute-from current-time #:optional (minute-list '()))
  (let ((time (localtime current-time)))
    (set-tm:sec   time 0)
    (bump-time time minute-list tm:min tm:hour set-tm:min set-tm:hour)))

(define* (next-second-from current-time #:optional (second-list '()))
  (let ((time (localtime current-time)))
    (bump-time time second-list tm:sec tm:min set-tm:sec set-tm:min)))

;;; The following procedures are convenient for configuration files.  They are
;;; wrappers for the next-X-from functions above, by implicitly using
;;; %CURRENT-ACTION-TIME as the time argument.

(define %current-action-time
  ;; The time a job was last run, the time from which the next time to run a
  ;; job must be computed. (When the program is first run, this time is set to
  ;; the configuration time so that jobs run from that moment forwards.) Once
  ;; we have this, we supply versions of the time computation commands above
  ;; which implicitly assume this value.
  (make-parameter 0))

(define* (next-year #:optional (args '()))
  "Compute the next year from %CURRENT-ACTION-TIME parameter object."
  (next-year-from (%current-action-time) args))

(define* (next-month #:optional (args '()))
  "Compute the next month from %CURRENT-ACTION-TIME parameter object."
  (next-month-from (%current-action-time) args))

(define* (next-day #:optional (args '()))
  "Compute the next day from %CURRENT-ACTION-TIME parameter object."
  (next-day-from (%current-action-time) args))

(define* (next-hour #:optional (args '()))
  "Compute the next hour from %CURRENT-ACTION-TIME parameter object."
  (next-hour-from (%current-action-time) args))

(define* (next-minute #:optional (args '()))
  "Compute the next minute from %CURRENT-ACTION-TIME parameter object."
  (next-minute-from (%current-action-time) args))

(define* (next-second #:optional (args '()))
  "Compute the next second from %CURRENT-ACTION-TIME parameter object."
  (next-second-from (%current-action-time) args))

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
;; %CURRENT-ACTION-TIME parameter object). A similar normalization is applied to
;; the action.
;;
;; Here we also compute the first time that the job is supposed to run, by
;; finding the next legitimate time from the current configuration time (set
;; right at the top of this program).

(define* (job time-proc action #:optional displayable
              #:key (user configuration-user))
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
         (cond (displayable displayable)
               ((procedure? action) "Lambda function")
               ((string? action) action)
               ((list? action) (with-output-to-string
                                 (lambda () (display action))))))
        (user* (if (or (string? user) (integer? user))
                   (getpw user)
                   user)))
    (add-job (lambda (current-time)
               (parameterize ((%current-action-time current-time))
                 ;; Allow for daylight savings time changes.
                 (let* ((next   (time-proc current-time))
                        (gmtoff (tm:gmtoff (localtime next)))
                        (d      (+ next
                                   (- gmtoff
                                      (tm:gmtoff (localtime current-time))))))
                   (if (eqv? (tm:gmtoff (localtime d)) gmtoff)
                       d
                       next))))
             action
             displayable
             configuration-time
             user*)))
