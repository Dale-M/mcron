;;;; core.scm -- alias for (mcron base) kept for backward compatibility
;;; Copyright © 2017 Mathieu Lirzin <mthl@gnu.org>
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

;;; TODO: Deprecate this alias in next major version.

(define-module (mcron core)
  #:use-module (mcron base)
  #:export (;; Deprecated
            get-schedule)
  #:re-export (add-job
               remove-user-jobs
               run-job-loop
               clear-environment-mods
               append-environment-mods
               ;; Deprecated and undocumented procedures.
               use-system-job-list
               use-user-job-list
               clear-system-jobs))

(define (get-schedule count)
  (with-output-to-string
    (lambda () (display-schedule count))))
