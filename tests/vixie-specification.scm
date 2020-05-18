;;;; vixie-specification.scm -- tests for (mcron vixie-specificaion) module
;;; Copyright © 2020 Mathieu Lirzin <mthl@gnu.org>
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

(use-modules (srfi srfi-1)
             (srfi srfi-64)
             (mcron vixie-specification))

(setenv "TZ" "UTC0")

;;; Do not send mail
(setenv "MAILTO" "")

(define (create-file! content)
  "Construct a temporary file port containing CONTENT which must be a string."
  (let ((port (mkstemp! (string-copy "file-XXXXXX"))))
    (display content port)
    (force-output port)
    port))

(define (clean-temp port)
  "Close and Delete a temporary file port"
  (let ((fname (port-filename port)))
    (close port)
    (delete-file fname)))

(define schedule (@@ (mcron base) %global-schedule))
(define schedule-user (@@ (mcron base) schedule-user))
(define set-schedule-user! (@@ (mcron base) set-schedule-user!))
(define job:environment (@@ (mcron base) job:environment))
(define job:displayable (@@ (mcron base) job:displayable))
(define job:user (@@ (mcron base) job:user))

(test-begin "vixie-specification")

;;; Parse user crontab file

(define user-crontab-example
  "# Example crontab
FOO=x
BAR=y

# Example of job definitions:
17 *	* * *	cd / && run baz
47 6	* * 7	foo -x /tmp/example || bar
")

(define user-crontab (create-file! user-crontab-example))

(dynamic-wind
  (const #t)
  (lambda ()
    (set-schedule-user! schedule '())
    (read-vixie-file (port-filename user-crontab))

    (test-assert "User schedule has exactly 2 matching jobs"
      (lset= string=?
             '("cd / && run baz"
               "foo -x /tmp/example || bar")
             (map job:displayable (schedule-user schedule))))

    (test-assert "Job environment matches configuration"
      (every (lambda (j)
               (lset= equal?
                      '(("FOO" . "x") ("BAR" . "y"))
                      (job:environment j)))
             (schedule-user schedule))))

  (lambda ()
    (clean-temp user-crontab)))

;;; Parse system crontab file

;;; Get two existing users from the test environment.
(setpwent)
(define user0 (getpwent))
(define user1 (or (getpwent) user0))
(define system-crontab-example
  (string-append
   "# Example crontab
BAZ=z

17 *	* * * " (passwd:name user0) " cd / && run baz
47 6	* * 7 "	(passwd:name user1) "   foo -x /tmp/example || bar"))

(define sys-crontab (create-file! system-crontab-example))

(dynamic-wind
  (const #t)
  (lambda ()
    (set-schedule-user! schedule '())
    (read-vixie-file (port-filename sys-crontab) parse-system-vixie-line)

    (test-assert "System schedule has exactly 2 matching jobs"
      (lset= equal?
             `((,user0 . "cd / && run baz")
               (,user1 . "foo -x /tmp/example || bar"))
             (map (lambda (j)
                    (cons (job:user j) (job:displayable j)))
                  (schedule-user schedule))))

    (test-assert "Job environment matches configuration"
    (every (lambda (j)
             (lset= equal? '(("BAZ" . "z")) (job:environment j)))
           (schedule-user schedule))))

  (lambda ()
    (clean-temp sys-crontab)))

;;; Try to parse a user crontab in a system context

(define wrong-system-crontab-example
  "
# Example of job definitions:
17 *	* * *	ls")

(define wrong-sys-crontab (create-file! wrong-system-crontab-example))

(dynamic-wind
  (const #t)
  (lambda ()
    (test-error "missing user"
      'mcron-error
      (read-vixie-file (port-filename wrong-sys-crontab)
                       parse-system-vixie-line)))

  (lambda ()
    (clean-temp wrong-sys-crontab)))

(test-end)
