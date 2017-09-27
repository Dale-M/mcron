;;;; vixie-specification.scm -- read Vixie-sytle configuration file
;;; Copyright © 2003 Dale Mellor <dale_mellor@users.sourceforge.net>
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

;;;; Commentary:
;;;
;;; Methods for reading a complete Vixie-style configuration file, either from
;;; a real file or an already opened port. It also exposes the method for
;;; parsing the time-specification part of a Vixie string, so that these can
;;; be used to form the next-time-function of a job in a Guile configuration
;;; file.
;;;
;;;; Code:

(define-module (mcron vixie-specification)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 rdelim)
  #:use-module (mcron base)
  #:use-module (mcron config)
  #:use-module (mcron job-specifier)
  #:use-module (mcron redirect)
  #:use-module (mcron vixie-time)
  #:use-module (srfi srfi-1)
  #:export (parse-user-vixie-line
            parse-system-vixie-line
            read-vixie-port
            read-vixie-file
            check-system-crontab))

;; A line in a Vixie-style crontab file which gives a command specification
;; carries two pieces of information: a time specification consisting of five
;; space-separated items, and a command which is also separated from the time
;; specification by a space. The line is broken into the two components, and the
;; job procedure run to add the two pieces of information to the job list (this
;; will in turn use the above function to turn the time specification into a
;; function for computing future run times of the command).

(define parse-user-vixie-line-regexp
  (make-regexp "^[[:space:]]*(([^[:space:]]+[[:space:]]+){5})(.*)$"))

(define (parse-user-vixie-line line)
  (let ((match (regexp-exec parse-user-vixie-line-regexp line)))
    (if (not match) 
        (throw 'mcron-error 10 "Bad job line in Vixie file."))
    (job (match:substring match 1)
         (lambda () (with-mail-out (match:substring match 3)))
         (match:substring match 3))))



;; The case of reading a line from /etc/crontab is similar to above but the user
;; ID appears in the sixth field, before the action.

(define parse-system-vixie-line-regexp
  (make-regexp (string-append "^[[:space:]]*(([^[:space:]]+[[:space:]]+){5})"
                              "([[:alpha:]][[:alnum:]_]*)[[:space:]]+(.*)$")))

(define (parse-system-vixie-line line)
  (let ((match (regexp-exec parse-system-vixie-line-regexp line)))
    (if (not match) 
        (throw 'mcron-error 11 "Bad job line in /etc/crontab."))
    (let ((user (match:substring match 3)))
      (set-configuration-user user)
      (job (match:substring match 1)
           (lambda () (with-mail-out (match:substring match 4)
                                     user))
           (match:substring match 4)))))



;; Procedure to act on an environment variable specification in a Vixie-style
;; configuration file, by adding an entry to the alist above. Returns #t if the
;; operation was successful, #f if the line could not be interpreted as an
;; environment specification.

(define parse-vixie-environment-regexp1
  (make-regexp
   "^[ \t]*([[:alpha:]_][[:alnum:]_]*)[ \t]*=[ \t]*\"(.*)\"[ \t]*$"))
(define parse-vixie-environment-regexp2
  (make-regexp
   "^[ \t]*([[:alpha:]_][[:alnum:]_]*)[ \t]*=[ \t]*'(.*)'[ \t]*$"))
(define parse-vixie-environment-regexp3
  (make-regexp
   "^[ \t]*([[:alpha:]_][[:alnum:]_]*)[ \t]*=[ \t]*(.*[^ \t])[ \t]*$"))
(define parse-vixie-environment-regexp4
  (make-regexp
   "^[ \t]*([[:alpha:]_][[:alnum:]_]*)[ \t]*=[ \t]*$"))


(define (parse-vixie-environment string)
  (let ((match (or (regexp-exec parse-vixie-environment-regexp1 string)
                   (regexp-exec parse-vixie-environment-regexp2 string)
                   (regexp-exec parse-vixie-environment-regexp3 string))))
    (if match
        (append-environment-mods (match:substring match 1)
                                 (match:substring match 2))
        (and=> (regexp-exec parse-vixie-environment-regexp4 string)
               (λ (match)
                 (append-environment-mods (match:substring match 1) #f))))))

;; The next procedure reads an entire Vixie-style file. For each line in the
;; file there are three possibilities (after continuation lines have been
;; appended): the line is blank or contains only a comment, the line contains an
;; environment modifier which will be handled in the mcron environment module,
;; or the line contains a command specification in which case we use the
;; procedure above to add an entry to the internal job list.
;;
;; Note that the environment modifications are cleared, so that there is no
;; interference between crontab files (this might lead to unpredictable
;; behaviour because the order in which crontab files are processed, if there is
;; more than one, is generally undefined).

(define read-vixie-file-comment-regexp
  (make-regexp "^[[:space:]]*(#.*)?$"))


(define (read-vixie-port port . parse-vixie-line)
  (clear-environment-mods)
  (if port
      (let ((parse-vixie-line
             (if (null? parse-vixie-line) parse-user-vixie-line
                 (car parse-vixie-line))))
        (do ((line (read-line port) (read-line port))
             (line-number 1 (1+ line-number)))
            ((eof-object? line))

          (let ((report-line line-number))
            ;; If the line ends with \, append the next line.
            (while (and (>= (string-length line) 1)
                        (char=? (string-ref line
                                            (- (string-length line) 1))
                                #\\))
                   (let ((next-line (read-line port)))
                     (if (eof-object? next-line)
                         (set! next-line ""))
                     (set! line-number (1+ line-number))
                     (set! line
                           (string-append
                            (substring line 0 (- (string-length line) 1))
                            next-line))))

            (catch 'mcron-error
                   (lambda ()
                     ;; Consider the three cases mentioned in the description.
                     (or (regexp-exec read-vixie-file-comment-regexp line)
                         (parse-vixie-environment line)
                         (parse-vixie-line line)))
                   (lambda (key exit-code . msg)
                     (throw 'mcron-error exit-code
                            (apply string-append
                                   (number->string report-line)
                                   ": "
                                   msg)))))))))



;; If a file cannot be opened, we must silently ignore it because it may have
;; been removed by crontab. However, if the file is there it must be parseable,
;; otherwise the error must be propagated to the caller.

(define (read-vixie-file file-path . parse-vixie-line)
  (let ((port #f))
    (catch #t (lambda () (set! port (open-input-file file-path)))
           (lambda (key . args) (set! port #f)))
    (if port
        (catch 'mcron-error
               (lambda ()
                 (if (null? parse-vixie-line)
                     (read-vixie-port port)
                     (read-vixie-port port (car parse-vixie-line)))
                 (close port))
               (lambda (key exit-code . msg)
                 (close port)
                 (throw 'mcron-error exit-code
                        (apply string-append file-path ":" msg)))))))


;; A procedure which determines if the /etc/crontab file has been recently
;; modified, and, if so, signals the main routine to re-read the file. We run
;; under the with-mail-to command so that the process runs as a child,
;; preventing lockup. If cron is supposed to check for updates to /etc/crontab,
;; then this procedure will be called about 5 seconds before every minute.

(define (check-system-crontab)
  (with-mail-out (lambda ()
                  (let ((mtime (stat:mtime (stat "/etc/crontab"))))
                    (if (> mtime (- (current-time) 60))
                        (let ((socket (socket AF_UNIX SOCK_STREAM 0)))
                          (connect socket AF_UNIX config-socket-file)
                          (display "/etc/crontab" socket)
                          (close socket)))))))
