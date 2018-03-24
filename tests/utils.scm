;;;; utils.scm -- tests for (mcron utils) module
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
             (mcron utils))

(test-begin "utils")

(define entry
  ;; Random user entry.
  (getpw))

;;; Call 'get-user' with a valid uid.
(let ((uid (getuid)))
  (test-equal "get-user: uid"
    uid
    (passwd:uid (get-user uid))))

;;; Call 'get-user' with a valid user name.
(let ((name (passwd:name entry)))
  (test-equal "get-user: name"
    name
    (passwd:name (get-user name))))

;;; Call 'get-user' with a passwd entry.
(test-equal "get-user: passwd entry"
  entry
  (get-user entry))

;;; Call 'get-user' with an invalid uid.
(test-error "get-user: invalid uid" #t (get-user -20000))

;;; Call 'get-user' with an invalid spec.
(test-error "get-user: invalid spec" #t (get-user 'wrong))

(test-end)
