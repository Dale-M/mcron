;;;; environment.scm -- tests for (mcron environment) module
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

(use-modules (srfi srfi-64)
             (mcron environment))

(test-begin "environment")

(test-assert "modifiy-environment: basic"
  (begin
    (modify-environment '(("FOO" . "bar")) (getpw))
    (equal? (getenv "FOO") "bar")))

(test-assert "modifiy-environment: user & logname"
  ;; Check that USER and LOGNAME environment variables can't be changed.
  (let* ((user-entry (pk (getpw)))
         (user-name  (passwd:name user-entry)))
    (modify-environment '(("USER" . "alice")) user-entry)
    (modify-environment '(("LOGNAME" . "bob")) user-entry)
    (equal? user-name
            (pk (getenv "USER"))
            (pk (getenv "LOGNAME")))))

(test-end)
