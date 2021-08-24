;;;; base.scm -- tests for (mcron base) module
;;; Copyright © 2018 Mathieu Lirzin <mthl@gnu.org>
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

(use-modules ((rnrs base) #:select (assert))
             (srfi srfi-64)
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
                         (action (lambda () "dummy action"))
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
(define child-cleanup (@@ (mcron base) child-cleanup))
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

;;; Check 'update-number-children!' constant value.
(test-equal "update-number-children!: set value"
  0
  (begin
    (update-number-children! (const 0))
    (unbox number-children)))

;;; Check 'run-job' and 'child-cleanup'.
;;; XXX: Having to use the filesystem for a unit test is wrong.
(let* ((filename (tmpnam))
       (action (lambda () (close-port (open-output-file filename))))
       (job (make-dummy-job #:user (getpw (getuid)) #:action action)))
  (dynamic-wind
    (const #t)
    (lambda ()
      (sigaction SIGCHLD (const #t))
      (let ((child-data (run-job job)))
        ;; Wait for the SIGCHLD signal sent when job exits.
        (pause)
        ;; Check 'run-job' result and if the number of children is up-to-date.
        (test-equal "run-job: basic"
          1
          (and (access? filename F_OK)
               (unbox number-children)))
        (child-cleanup (list child-data)))
      ;; Check that 'child-cleanup' updates the number of children.
      (test-equal "child-cleanup: one" 0 (unbox number-children)))
    (lambda ()
      (and (access? filename F_OK) (delete-file filename))
      (sigaction SIGCHLD SIG_DFL))))

(define (dummy-job/capture-output action)
  "Return the output of a dummy-job that ran ACTION."
  (with-output-to-string
    (lambda ()
      (dynamic-wind
        (const #t)
        (lambda ()
          (sigaction SIGCHLD (const #t))
          (let ((child-data
                 (run-job
                  (make-dummy-job
                   #:user (getpw (getuid))
                   #:action action))))
            (pause)
            (child-cleanup (list child-data))))
        (lambda ()
          #t
          (sigaction SIGCHLD SIG_DFL))))))

(test-assert "run-job, output"
  (let ((output (dummy-job/capture-output
                 (lambda ()
                   (format #t "output line 1~%")
                   (format #t "output line 2\nand 3~%")
                   (system "echo poutine")
                   (format (current-error-port)
                           "some error~%")))))
    (assert (string-contains output "dummy: running"))
    (assert (string-contains output "dummy: output line 1"))
    (assert (string-contains output "dummy: and 3"))
    (assert (string-contains output "dummy: poutine"))
    (assert (string-contains output "dummy: some error"))
    (assert (string-contains output "dummy: completed in"))))

(test-assert "validate-date-format, valid"
  (validate-date-format "~1"))

(test-assert "validate-date-format, invalid"
  (catch 'mcron-error
    (lambda ()
      (validate-date-format "~¾")
      #f)
    (const #t)))

(test-assert "validate-log-format, valid"
  (validate-log-format "the message only: ~3@*~a~%"))

(test-assert "validate-log-format, invalid"
  (catch 'mcron-error
    (lambda ()
      ;; There aren't that many arguments!
      (validate-log-format "~20@*~a~%")
      #f)
    (const #t)))

(test-assert "run-job, output with custom format"
  (let ((output (parameterize ((%log-format "the message only: ~3@*~a~%"))
                  (dummy-job/capture-output
                   (lambda ()
                     (format #t "output line 1~%"))))))
    (string-contains output "the message only: output line 1\n")))

(test-assert "run-job, failure"
  (let ((output (dummy-job/capture-output
                 (lambda ()
                   (error "that didn't go well")))))
    (assert (string-contains output "that didn't go well"))
    (assert (string-contains output "failed after"))))

(test-assert "run-job, failure in shell action"
  (let ((output (dummy-job/capture-output
                 (lambda ()
                   (system "exit 1")))))
    (assert (string-contains output "unclean exit status"))
    (assert (string-contains output "failed after"))))

(test-end)
