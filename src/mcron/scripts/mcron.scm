;;;; mcron -- run jobs at scheduled times
;;; Copyright © 2003, 2012 Dale Mellor <dale_mellor@users.sourceforge.net>
;;; Copyright © 2015, 2016, 2018 Mathieu Lirzin <mthl@gnu.org>
;;; Copyright © 2021 Maxim Cournoyer <maxim.cournoyer@gmail.com>
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

(define-module (mcron scripts mcron)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 local-eval)
  #:use-module (ice-9 rdelim)
  #:use-module (mcron base)
  #:use-module (mcron config)
  #:use-module (mcron job-specifier)    ; For user/system files.
  #:use-module (mcron utils)
  #:use-module (mcron vixie-specification)
  #:export (main))



(define (show-help)
  (display "Usage: mcron [OPTION...] [FILE...]
Run an mcron process according to the specifications in the FILE... (`-' for
standard input), or use all the files in ~/.config/cron (or the deprecated
~/.cron) with .guile or .vixie extensions.

  -d, --daemon               Run as a daemon process
  -i, --stdin=(guile|vixie)  Format of data passed as standard input
                             (default guile)
  -s, --schedule[=N]         Display the next N (or 8) jobs that will be run
  --log-format=FMT           (ice-9 format) format string for log messages
  --date-format=FMT          (srfi srfi-19) date format string for log messages
  -?, --help                 Give this help list
  -V, --version              Print program version

Mandatory or optional arguments to long options are also mandatory or optional
for any corresponding short options.

Report bugs to bug-mcron@gnu.org.

"))



(define process-user-file
  (let ((guile-regexp (make-regexp "\\.gui(le)?$"))
        (vixie-regexp (make-regexp "\\.vix(ie)?$")))
    (lambda* (file-name #:optional guile-syntax? #:key (input "guile"))
      "Process FILE-NAME according its extension.  When GUILE-SYNTAX? is TRUE,
force guile syntax usage.  If FILE-NAME format is not recognized, it is
silently ignored."
      (cond ((string=? "-" file-name)
                  (if (string=? input "vixie")
                      (read-vixie-port (current-input-port))
                      (eval-string (read-string)
                                   (resolve-module '(mcron job-specifier)))))
            ((or guile-syntax? (regexp-exec guile-regexp file-name))
                  (eval-string (read-delimited "" (open-input-file file-name))
                               (resolve-module '(mcron job-specifier))))
            ((regexp-exec vixie-regexp file-name)
                  (read-vixie-file file-name))))))



(define (process-files-in-user-directory input-type)
  "Process files in $XDG_CONFIG_HOME/cron and/or ~/.cron directories (if
$XDG_CONFIG_HOME is not defined uses ~/.config/cron instead)."
  (let ((errors 0)
        (home-directory (passwd:dir (getpw (getuid)))))
    (map (λ (dir)
           (catch #t
             (λ ()
               (for-each (λ (file)
                           (process-user-file (string-append dir "/" file)
                                              #:input input-type))
                         (scandir dir)))
             (λ (key . args)
               (set! errors (1+ errors)))))
         (list (string-append home-directory "/.cron")
               (string-append (or (getenv "XDG_CONFIG_HOME")
                                  (string-append home-directory "/.config"))
                              "/cron")))
    (when (eq? 2 errors)
      (mcron-error 13
        "There was an error reading files in your ~/.config/cron (or ~/.cron)
directory. Double-check the folder and file permissions and syntax."))))



(define (%process-files files input-type)
  (if (null? files)
      (process-files-in-user-directory input-type)
      (for-each (λ (file) (process-user-file file #t)) files)))


;;;
;;; Entry point.
;;;

(define (main)

  (let ((options
            (getopt-long
                (command-line)
                `((daemon   (single-char #\d) (value #f))
                  (stdin    (single-char #\i) (value #t)
                            (predicate ,(λ (in) (or (string=? in "guile")
                                                    (string=? in "vixie")))))
                  (schedule (single-char #\s) (value optional)
                            (predicate ,string->number))
                  (log-format  (value #t) (predicate ,validate-log-format))
                  (date-format (value #t) (predicate ,validate-date-format))
                  (help     (single-char #\?))
                  (version  (single-char #\V))))))

    (cond ((option-ref options 'help #f)      (show-help)             (exit 0))
          ((option-ref options 'version #f)   (show-version "mcron")  (exit 0)))
  
    (when config-debug
      (debug-enable 'backtrace))

    (%process-files (option-ref options '() '())
                    (option-ref options 'stdin "guile"))

    (cond ((option-ref options 'schedule #f)
               => (λ (count)
                     (let ((c (if (string? count) (string->number count) 8)))
                       (display-schedule  (if (exact-integer? c) (max 1 c) 8)))
                     (exit 0)))
          ((option-ref options 'daemon #f)
               (case (primitive-fork)
                     ((0)  (setsid))
                     (else (exit 0)))))

    (parameterize
        ((%log-format  (option-ref options 'log-format (%log-format)))
         (%date-format (option-ref options 'date-format (%date-format))))
      (run-job-loop))))
