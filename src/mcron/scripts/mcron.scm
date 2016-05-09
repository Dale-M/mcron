;;;; mcron -- run jobs at scheduled times
;;; Copyright © 2003, 2012 Dale Mellor <dale_mellor@users.sourceforge.net>
;;; Copyright © 2015, 2016 Mathieu Lirzin <mthl@gnu.org>
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
  #:use-module (mcron base)
  #:use-module (mcron config)
  #:use-module (mcron job-specifier)    ;for user/system files
  #:use-module (mcron main)
  #:use-module (mcron vixie-specification)
  #:export (main))

(define (show-help)
  (display "Usage: mcron [OPTIONS] [FILES]
Run an mcron process according to the specifications in the FILES (`-' for
standard input), or use all the files in ~/.config/cron (or the
deprecated ~/.cron) with .guile or .vixie extensions.

  -v, --version             Display version
  -h, --help                Display this help message
  -sN, --schedule[=]N       Display the next N jobs that will be run by mcron
  -d, --daemon              Immediately detach the program from the terminal
                              and run as a daemon process
  -i, --stdin=(guile|vixie) Format of data passed as standard input or
                              file arguments (default guile)")
  (newline)
  (show-package-information))

(define %options
  `((schedule (single-char #\s) (value #t)
              (predicate ,(λ (str) (string->number str))))
    (daemon   (single-char #\d) (value #f))
    (noetc    (single-char #\n) (value #f))
    (stdin    (single-char #\i) (value #t)
              (predicate ,(λ (val)
                            (or (string=? val "guile")
                                (string=? val "vixie")))))
    (version  (single-char #\v) (value #f))
    (help     (single-char #\h) (value #f))))

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
                 (eval-string (stdin->string))))
            ((or guile-syntax? (regexp-exec guile-regexp file-name))
             (load file-name))
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
               (for-each-file
                (λ (file)
                  (process-user-file (string-append dir "/" file)
                                     #:input input-type))
                dir))
             (λ (key . args)
               (set! errors (1+ errors)))))
         (list (string-append home-directory "/.cron")
               (string-append (or (getenv "XDG_CONFIG_HOME")
                                  (string-append home-directory "/.config"))
                              "/cron")))
    (when (eq? 2 errors)
      (mcron-error 13
        "Cannot read files in your ~/.config/cron (or ~/.cron) directory."))))

(define (%process-files files input-type)
  (if (null? files)
      (process-files-in-user-directory input-type)
      (for-each (λ (file) (process-user-file file #t)) files)))


;;;
;;; Entry point.
;;;

(define* (main #:optional (args (command-line)))
  (let ((opts (parse-args args %options)))
    (when config-debug
      (debug-enable 'backtrace))
    (cond ((option-ref opts 'help #f)
           (show-help)
           (exit 0))
          ((option-ref opts 'version #f)
           (show-version "mcron")
           (exit 0))
          (else
           (%process-files (option-ref opts '() '())
                           (option-ref opts 'stdin "guile"))
           (cond ((option-ref opts 'schedule #f) ;display jobs schedule
                  => (λ (count)
                       (display (get-schedule (max 1 (string->number count))))
                       (exit 0)))
                 ((option-ref opts 'daemon #f) ;run mcron as a daemon
                  (case (primitive-fork)
                    ((0)  (setsid))
                    (else (exit 0)))))
           ;; Forever execute the 'run-job-loop', and when it drops out (can
           ;; only be because a message has come in on the socket) we process
           ;; the socket request before restarting the loop again.
           (catch-mcron-error
            (let ((fdes-list '()))
              (while #t
                (run-job-loop fdes-list)
                ;; we can also drop out of run-job-loop because of a SIGCHLD,
                ;; so must test FDES-LIST.
                (unless (null? fdes-list)
                  (process-update-request fdes-list)))))))))
