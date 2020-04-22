;;;; job-specifier.scm -- tests for (mcron job-specifier) module
;;; Copyright © 2016 Mathieu Lirzin <mthl@gnu.org>
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

(use-modules (ice-9 match)
             (srfi srfi-64)
             (srfi srfi-111)
             (mcron job-specifier))

(test-begin "job-specifier")

(test-equal "range: basic"
  '(0 1 2 3 4 5 6 7 8 9)
  (range 0 10))

(test-equal "range: positive step"
  '(0 2 4 6 8)
  (range 0 10 2))

(test-assert "range: zero step"
  ;; Since this behavior is undefined, only check if range doesn't crash.
  (range 0 5 0))

(test-assert "range: negative step"
  ;; Since this behavior is undefined, only check if range doesn't crash.
  (range 0 5 -2))

(test-assert "range: reverse boundaries"
  (range 10 3))

(define %find-best-next (@@ (mcron job-specifier) %find-best-next))

(test-assert "%find-best-next: exact"
  ;; Ensure that '%find-best-next' preserves the exactness of the numbers
  ;; inside the NEXT-LIST argument.
  (match (pk 'match (%find-best-next 1 '(0 2)))
    ((a . b) (and (exact? a) (exact? b)))))

;;;
;;; Check 'next-...' procedures.
;;;

;;; TODO: Find more meaningful date examples.

(setenv "TZ" ":UTC")

(test-equal "next-year"
  (list 1893456000 1546300800)
  (list (next-year '(130))   ;; This is the year 2030.
        (next-year-from 1522095469)))

(test-equal "next-month"
  5097600
  (next-month-from 101 '(0 2 4)))

(test-equal "next-day"
  345600
  (next-day-from 4337 '(0 5 10)))

(test-equal "next-hour"
  3600
  (next-hour-from 3 '(0 1 2 3 4)))

(test-equal "next-minute"
  60
  (next-minute-from 8))

(test-equal "next-second"
  15
  (next-second-from 14))

;;;
;;; Check 'configuration-user' manipulation
;;;

(define configuration-user (@@ (mcron job-specifier) configuration-user))

;;; Call 'set-configuration-user' with a valid uid.
(let ((uid (getuid)))
  (test-equal "set-configuration-user: uid"
    uid
    (begin
      (set-configuration-user uid)
      (passwd:uid (unbox configuration-user)))))

(define entry
  ;; Random user entry.
  (getpw))

;;; Call 'set-configuration-user' with a valid user name.
(let ((name (passwd:name entry)))
  (test-equal "set-configuration-user: name"
    name
    (begin
      (set-configuration-user name)
      (passwd:name (unbox configuration-user)))))

(define root-entry (getpw 0))

;;; Call 'set-configuration-user' with a passwd entry.
(test-equal "set-configuration-user: passwd entry"
  root-entry
  (begin
    (set-configuration-user root-entry)
    (unbox configuration-user)))

;;; Call 'set-configuration-user' with an invalid uid.
(test-error "set-configuration-user: invalid uid"
   #t
   (set-configuration-user -20000))

;;; Call 'set-configuration-user' with an invalid spec.
(test-error "set-configuration-user: invalid spec"
   #t
   (set-configuration-user 'wrong))

;;;
;;; Check the 'job' procedure
;;;

(test-assert "job: procedure timeproc"
  (job 1+ "dummy action"))

(test-assert "job: list timeproc"
  (job '(next-hour '(0)) "dummy action"))

(test-assert "job: string timeproc"
  (job "30 4 1,15 * 5" "dummy action"))

(test-error "job: invalid string timeproc"
  'mcron-error
  (job "30 4 1,15 * WRONG" "dummy action"))

(test-error "job: invalid timeproc"
  'mcron-error
  (job 42 "dummy action"))

(test-assert "job: procedure action"
  (job 1+ (λ () (display "hello\n"))))

(test-assert "job: list action"
  (job 1+ '(display "hello\n")))

(test-assert "job: string action"
  (job 1+ "echo hello"))

(test-error "job: string action"
  'mcron-error
  (job 1+ 42))

(test-assert "job: user name"
  (job 1+ "dummy action" #:user (getuid)))

(test-end)
