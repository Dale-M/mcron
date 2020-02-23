;;;; utils.scm -- helper procedures
;;; Copyright © 2003, 2012 Dale Mellor <dale_mellor@users.sourceforge.net>
;;; Copyright © 2015, 2016, 2018 Mathieu Lirzin <mthl@gnu.org>
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

(define-module (mcron utils)
  #:use-module (ice-9 rdelim)
  #:use-module (mcron config)
  #:use-module (mcron base)
  #:use-module (mcron job-specifier)
  #:use-module (mcron vixie-specification)
  #:export (catch-mcron-error
            mcron-error
            show-version
            show-package-information
            process-update-request
            get-user)
  #:re-export (read-string))

(define (mcron-error exit-code . rest)
  "Print an error message (made up from the parts of REST), and if the
EXIT-CODE error is fatal (present and non-zero) then exit to the system with
EXIT-CODE."
  (with-output-to-port (current-error-port)
    (lambda ()
      (for-each display (cons "mcron: " rest))
      (newline)))
  (when (and exit-code (not (eq? exit-code 0)))
    (primitive-exit exit-code)))

(define-syntax-rule (catch-mcron-error exp ...)
  "Evaluate EXP .... if an 'mcron-error exception occurs, print its diagnostics
and exit with its error code."
  (catch 'mcron-error
    (lambda () exp ...)
    (lambda (key exit-code . msg)
      (apply mcron-error exit-code msg))))

(define (show-version command)
  "Display version information for COMMAND and quit."
  (let* ((name       config-package-name)
         (short-name (cadr (string-split name #\space)))
         (version    config-package-version))
    (simple-format #t "~a (~a) ~a
Copyright (C) 2020 the ~a authors.
License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>
This is free software: you are free to change and redistribute it.
There is NO WARRANTY, to the extent permitted by law.\n"
		   command name version short-name)))

(define (show-package-information)
  "Display where to get help and send bug reports."
  (simple-format #t "\nReport bugs to: ~a.
~a home page: <~a>
General help using GNU software: <http://www.gnu.org/gethelp/>\n"
		 config-package-bugreport
		 config-package-name
		 config-package-url))

(define (process-update-request fdes-list)
  "Read a user name from the socket, dealing with the /etc/crontab special
case, remove all the user's jobs from the job list, and then re-read the
user's updated file.  In the special case drop all the system jobs and re-read
the /etc/crontab file.  This function should be called whenever a message
comes in on the above socket."
  (let* ((sock      (car (accept (car fdes-list))))
         (user-name (read-line sock)))
    (close sock)
    (set-configuration-time (current-time))
    (catch-mcron-error
     (if (string=? user-name "/etc/crontab")
         (begin
           (clear-system-jobs)
           (use-system-job-list)
           (read-vixie-file "/etc/crontab" parse-system-vixie-line)
           (use-user-job-list))
         (let ((user (getpw user-name)))
           (remove-user-jobs user)
           (set-configuration-user user)
           (read-vixie-file (string-append config-spool-dir "/" user-name)))))))

(define (get-user spec)
  "Return the passwd entry corresponding to SPEC.  If SPEC is passwd entry
then return it.  If SPEC is not a valid specification throw an exception."
  (cond ((or (string? spec) (integer? spec))
         (getpw spec))
        ((vector? spec)                 ;assume a user passwd entry
         spec)
        (else
         (throw 'invalid-user-specification spec))))
