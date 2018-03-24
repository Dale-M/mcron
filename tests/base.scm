;;;; base.scm -- tests for (mcron base) module
;;; Copyright © 2018 Mathieu Lirzin <mthl@gnu.org>
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

(use-modules (srfi srfi-64)
             (srfi srfi-111)
             (mcron base))

(test-begin "base")

(setlocale LC_ALL "C")
(setenv "TZ" "UTC0")

;;; Import private procedures.
(define make-schedule (@@ (mcron base) make-schedule))
(define schedule-current (@@ (mcron base) schedule-current))
(define schedule-user (@@ (mcron base) schedule-user))
(define schedule-system (@@ (mcron base) schedule-system))
(define make-job (@@ (mcron base) make-job))
(define find-next-jobs (@@ (mcron base) find-next-jobs))

(define %user0 #("user0" "x" 0 0 "user0" "/var/empty" "/bin/sh"))
(define %user1 #("user1" "x" 1 1 "user1" "/var/empty" "/bin/sh"))

(define* (make-dummy-job #:optional (displayable "dummy")
                         #:key
                         (user (getpw))
                         (time-proc 1+)
                         (action (λ () "dummy action"))
                         (environment '())
                         (next-time 0))
  (make-job user time-proc action environment displayable next-time))

;;; Check 'use-system-job-list' and 'use-user-job-list' effect
(let ((schdl (make-schedule '() '() 'user)))
  (use-system-job-list #:schedule schdl)
  (test-eq "use-system-job-list"
    'system
    (schedule-current schdl))

  (use-user-job-list #:schedule schdl)
  (test-eq "use-user-job-list"
    'user
    (schedule-current schdl)))

;;; Check that 'remove-user-jobs' with only one type of user job clears the
;;; schedule.
(let* ((job (make-dummy-job #:user %user0))
       (schdl (make-schedule (list job) '() 'user)))
  (remove-user-jobs %user0 #:schedule schdl)
  (test-equal "remove-user-jobs: only one"
    '()
    (schedule-user schdl)))

;;; Check that 'remove-user-jobs' with only two types of user jobs keep the
;;; other user jobs in the schedule.
(let* ((job0 (make-dummy-job #:user %user0))
       (job1 (make-dummy-job #:user %user1))
       (schdl (make-schedule (list job0 job1) '() 'user)))
  (remove-user-jobs %user0 #:schedule schdl)
  (test-equal "remove-user-jobs: keep one"
    (list job1)
    (schedule-user schdl)))

;;; Check that 'clear-system-jobs' removes all system jobs and has no effect
;;; on the user jobs.
(let* ((job0 (make-dummy-job #:user %user0))
       (job1 (make-dummy-job #:user %user1))
       (schdl (make-schedule (list job0) (list job1) 'user)))
  (clear-system-jobs #:schedule schdl)
  (test-assert "clear-system-jobs: basic"
    (and (equal? (list job0) (schedule-user schdl))
         (equal? '() (schedule-system schdl)))))

;;; Check that 'add-job' adds a user job when the current schedule is 'user.
(let ((schdl (make-schedule '() '() 'user)))
  (add-job 1+ (const #t) "job0" 0 "user" #:schedule schdl)
  (test-assert "add-job: user schedule"
    (and (= 1 (length (schedule-user schdl)))
         (= 0 (length (schedule-system schdl))))))

;;; Check that 'add-job' adds a system job when the current schedule is
;;; 'system.
(let ((schdl (make-schedule '() '() 'system)))
  (add-job 1+ (const #t) "job0" 0 "user" #:schedule schdl)
  (test-assert "add-job: system schedule"
    (and (= 0 (length (schedule-user schdl)))
         (= 1 (length (schedule-system schdl))))))

;;; Check that 'find-next-jobs' find the soonest job.
(let* ((job0 (make-dummy-job #:user %user0 #:next-time 5))
       (job1 (make-dummy-job #:user %user1 #:next-time 10))
       (schdl (make-schedule (list job0) (list job1) 'user)))
  (test-equal "find-next-jobs: basic"
    (list 5 job0)
    (find-next-jobs #:schedule schdl)))

;;; Check that 'find-next-jobs' can find multiple soonest jobs.
(let* ((job0 (make-dummy-job #:user %user0 #:next-time 5))
       (job1 (make-dummy-job #:user %user1 #:next-time 5))
       (schdl (make-schedule (list job0) (list job1) 'user)))
  (test-equal "find-next-jobs: two jobs"
    (list 5 job0 job1)
    (find-next-jobs #:schedule schdl)))

;;; Check that 'find-next-jobs' returns #f when the schedule is empty.
(let ((schdl (make-schedule '() '() 'user)))
  (test-equal "find-next-jobs: empty"
    (list #f)
    (find-next-jobs #:schedule schdl)))

;;; Check output of 'display-schedule' with a basic schedule.
(test-assert "display-schedule: basic"
  (and (equal?
        "Thu Jan  1 00:00:05 1970 +0000\ndummy\n\n"
        (let* ((job0 (make-dummy-job #:user %user0 #:next-time 5))
               (job1 (make-dummy-job #:user %user1 #:next-time 10))
               (schdl (make-schedule (list job0) (list job1) 'user)))
          (with-output-to-string
            (λ () (display-schedule 1 #:schedule schdl)))))
       (equal?
        (string-append
         "Thu Jan  1 00:00:05 1970 +0000\ndummy\n\n"
         "Thu Jan  1 00:00:06 1970 +0000\ndummy\n\n")
        (let* ((job0 (make-dummy-job #:user %user0 #:next-time 5))
               (job1 (make-dummy-job #:user %user1 #:next-time 10))
               (schdl (make-schedule (list job0) (list job1) 'user)))
          (with-output-to-string
            (λ () (display-schedule 2 #:schedule schdl)))))))

;;; Check output of 'display-schedule' with an empty schedule.
(let ((schdl (make-schedule '() '() 'user)))
  (test-equal "display-schedule: empty"
    ""
    (with-output-to-string
      (λ () (display-schedule 1 #:schedule schdl)))))

;;;
;;; Running jobs
;;;

;;; Import private global.
(define number-children (@@ (mcron base) number-children))

;;; Import private procedures.
(define update-number-children! (@@ (mcron base) update-number-children!))
(define run-job (@@ (mcron base) run-job))

;;; Check 'number-children' initial value.
(test-equal "number-children: init"
  0
  (unbox number-children))

;;; Check 'update-number-children!' incrementation.
(test-equal "update-number-children!: 1+"
  2
  (begin
    (update-number-children! 1+)
    (update-number-children! 1+)
    (unbox number-children)))

;;; Check 'update-number-children!' decrementation.
(test-equal "update-number-children!: 1-"
  1
  (begin
    (update-number-children! 1-)
    (unbox number-children)))

;;; Check 'run-job' basic call.
;;; XXX: Having to use the filesystem for a unit test is wrong.
(let* ((filename (tmpnam))
       (action (λ () (close-port (open-output-file filename))))
       (job (make-dummy-job #:user (getpw (getuid)) #:action action)))
  (dynamic-wind
    (λ ()
      (run-job job)
      (waitpid WAIT_ANY))
    (λ ()
      (test-assert "run-job: basic"
        (access? filename F_OK)))
    (λ ()
      (delete-file filename))))

(test-end)
