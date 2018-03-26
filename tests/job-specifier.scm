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

(test-equal "next-hour-from"
  7200
  (next-hour-from 10 '(0 3 7)))

(test-end)
