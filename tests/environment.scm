;;;; environment.scm -- tests for (mcron environment) module
;;; Copyright © 2016, 2018 Mathieu Lirzin <mthl@gnu.org>
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
             (mcron environment))

(test-begin "environment")

;;; Check 'current-environment-mods' initial value which should be empty.
(test-equal "current-environment-mods: init"
  '()
  (unbox (@@ (mcron environment) %current-environment-mods)))

;;; Check 'current-environment-mods-copy' with an empty environment
(test-assert "current-environment-mods-copy: empty"
  (let* ((env (box '()))
         (copy0 (get-current-environment-mods-copy #:environ env))
         (copy1 (get-current-environment-mods-copy #:environ env)))
    (set! copy1 (assoc-set! copy1 "FOO" "BAR"))
    (and (equal? '() (unbox env))
         (equal? '() copy0)
         (equal? '(("FOO" . "BAR")) copy1))))

;;; Check 'current-environment-mods-copy' with a basic environment
(test-assert "current-environment-mods-copy: basic"
  (let* ((init-env '(("a" . "1") ("b" . "2")))
         (env (box init-env))
         (copy0 (get-current-environment-mods-copy #:environ env))
         (copy1 (get-current-environment-mods-copy #:environ env)))
    (set! copy1 (assoc-set! copy1 "c" "3"))
    (and (equal? init-env (unbox env))
         (equal? init-env copy0)
         (equal? `(("c" . "3") . ,init-env) copy1))))

;;; Check 'append-environment-mods' basic call
(test-equal "append-environment-mods: basic"
  "BAR"
  (let ((env (box '())))
    (append-environment-mods "FOO" "BAR" #:environ env)
    (assoc-ref (unbox env) "FOO")))

;;; Check 'append-environment-mods' that when adding the same key twice the
;;; later is placed after the previous one.
(test-equal "append-environment-mods: twice"
  '(("FOO" . "BAR") ("FOO" . "BAZ"))
  (let ((env (box '())))
    (append-environment-mods "FOO" "BAR" #:environ env)
    (append-environment-mods "FOO" "BAZ" #:environ env)
    (unbox env)))

;;; Check 'clear-environment-mods' side effect
(test-equal "clear-environment-mods: effect"
  '()
  (let ((env (box '())))
    (append-environment-mods "FOO" "BAR" #:environ env)
    (append-environment-mods "FOO" "BAZ" #:environ env)
    (clear-environment-mods #:environ env)
    (unbox env)))

;;; Check 'modify-environment' basic call
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
