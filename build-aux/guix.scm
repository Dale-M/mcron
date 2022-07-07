;;;; guix.scm -- Guix package definition
;;; Copyright © 2016 Mathieu Lirzin <mthl@gnu.org>
;;; Copyright © 2018 宋文武 <iyzsong@member.fsf.org>
;;; Copyright © 2019 Efraim Flashner <efraim@flashner.co.il>
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

(use-modules (ice-9 popen)
             (ice-9 rdelim)
             (gnu)
             (guix)
             (srfi srfi-1))

(define (keep-mcron-file? file stat)
  ;; Return #t if FILE in Mcron repository must be kept, #f otherwise. FILE
  ;; is an absolute file name and STAT is the result of 'lstat' applied to
  ;; FILE.
  (not (or (any (λ (str) (string-contains file str))
                '(".git" "autom4te" "Makefile.in" ".go" ".log"
                  "stamp-vti" ".dirstamp"))
           (any (λ (str) (string-suffix? str file))
                '("trs" "configure" "Makefile" "config.status" "pre-inst-env"
                  "aclocal.m4" "bin/cron" "bin/mcron" "bin/crontab"
                  "config.cache" "guix.scm")))))

(define %srcdir
  (or (current-source-directory) "."))

(package
  (inherit (specification->package "mcron"))
  (version "1.2.1")
  (source (local-file (dirname %srcdir) #:recursive? #t
                      #:select? keep-mcron-file?))
  (inputs
   `(("guile" ,(specification->package "guile@3"))))
  (native-inputs
   `(("autoconf" ,(specification->package "autoconf"))
     ("automake" ,(specification->package "automake"))
     ("help2man" ,(specification->package "help2man"))
     ("pkg-config" ,(specification->package "pkg-config"))
     ("texinfo" ,(specification->package "texinfo"))
     ("tzdata" ,(specification->package "tzdata")))))
