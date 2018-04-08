;;;; vixie-time.scm -- tests for (mcron vixie-time) module
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

(use-modules (srfi srfi-1)
             (srfi srfi-64)
             (mcron vixie-time))

(setenv "TZ" "UTC0")

(test-begin "vixie-time")

(define (times-equal spec times proc)
  (test-equal spec
    (cdr times)
    (fold-right (λ (val acc)
                  (cons (proc val) acc))
                '()
                (drop-right times 1))))

(times-equal
 "every minute"
 '(0 60 120 180 240 300 360 420)
 (parse-vixie-time "* * * * *"))

(times-equal
 "every hour"
 (list 0
       3600
       (* 2 3600)
       (* 3 3600)
       (* 4 3600)
       (* 5 3600)
       (* 6 3600)
       (* 7 3600))
 (parse-vixie-time "0 * * * *"))

(times-equal
 "every day"
 (list 0
       (* 24 3600)
       (* 2 24 3600)
       (* 3 24 3600)
       (* 4 24 3600)
       (* 5 24 3600)
       (* 6 24 3600)
       (* 7 24 3600))
 (parse-vixie-time "0 0 * * *"))

(times-equal
 "every month"
 (list 0
       (* 31 86400)                        ;jan
       (* (+ 31 28) 86400)                 ;fev
       (* (+ 31 28 31) 86400)              ;mar
       (* (+ 31 28 31 30) 86400)           ;avr
       (* (+ 31 28 31 30 31) 86400)        ;may
       (* (+ 31 28 31 30 31 30) 86400)     ;jun
       (* (+ 31 28 31 30 31 30 31) 86400)) ;july
 (parse-vixie-time "0 0 1 * *"))

(times-equal
 "every year"
 (list 0
       (* 365 86400)                      ;1971
       (* 2 365 86400)                    ;1972 (leap)
       (* (+ (* 2 365) 366) 86400)        ;1973
       (* (+ (* 3 365) 366) 86400)        ;1974
       (* (+ (* 4 365) 366) 86400)        ;1975
       (* (+ (* 5 365) 366) 86400)        ;1976 (leap)
       (* (+ (* 5 365) (* 2 366)) 86400)) ;1977
 (parse-vixie-time "0 0 1 0 *"))

(times-equal
 "30 4 1,15 * 5"
 (list 0
       (+ (* 4 3600) 1800)
       (+ (* 28 3600) 1800)
       (+ (* 8 86400) (* 4 3600) 1800)
       (+ (* 13 86400) (* 28 3600) 1800)
       (+ (* 15 86400) (* 4 3600) 1800)
       (+ (* 532 3600) 1800))
 (parse-vixie-time "30 4 1,15 * 5"))

;;;
;;; Errors
;;;

;; FIXME: infinite loop
;; (test-error "month 0" #t
;;   (let ((p (parse-vixie-time "0 0 0 * *")))
;;     (p 1234)))

(test-error
 "not enough fields"
 'mcron-error
 (parse-vixie-time "1 2 3 4"))

(test-error
 "too many fields"
 'mcron-error
 (parse-vixie-time "1 2 3 4 5 6"))

(test-end)
