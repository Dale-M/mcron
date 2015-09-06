;;   Copyright (C) 2015, 2016 Mathieu Lirzin
;;   Copyright (C) 2003, 2012 Dale Mellor
;; 
;;   This file is part of GNU mcron.
;;
;;   GNU mcron is free software: you can redistribute it and/or modify it under
;;   the terms of the GNU General Public License as published by the Free
;;   Software Foundation, either version 3 of the License, or (at your option)
;;   any later version.
;;
;;   GNU mcron is distributed in the hope that it will be useful, but WITHOUT
;;   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;;   FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;;   more details.
;;
;;   You should have received a copy of the GNU General Public License along
;;   with GNU mcron.  If not, see <http://www.gnu.org/licenses/>.

;;; This is the 'main' routine for the whole system; this module is the global
;;; entry point (after the minimal C wrapper); to all intents and purposes the
;;; program is pure Guile and starts here.

(define-module (mcron main)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (mcron config)
  #:use-module (mcron core)
  #:use-module (mcron job-specifier)
  #:use-module (mcron vixie-specification)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-26)
  #:export (delete-run-file
	    main))

(define* (command-name #:optional (command (car (command-line))))
  "Extract the actual command name from COMMAND.  This returns the last part
of COMMAND without any non-alphabetic characters.  For example \"in.cron\" and
\"./mcron\" will return respectively \"cron\" and \"mcron\".

When COMMAND is not specified this uses the first element of (command-line)."
  (match:substring (regexp-exec (make-regexp "[[:alpha:]]*$") command)))

(define (mcron-error exit-code . rest)
  "Print an error message (made up from the parts of REST), and if the
EXIT-CODE error is fatal (present and non-zero) then exit to the system with
EXIT-CODE."
  (with-output-to-port (current-error-port)
    (lambda ()
      (for-each display (append (list (command-name) ": ") rest))
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

(define command-type
  ;; We will be doing a lot of testing of the command name, so it makes sense
  ;; to perform the string comparisons once and for all here.
  (let* ((command   (command-name))
         (command=? (cut string=? command <>)))
    (cond ((command=? "mcron") 'mcron)
          ((or (command=? "cron") (command=? "crond")) 'cron)
          ((command=? "crontab") 'crontab)
          (else (mcron-error 12 "The command name is invalid.")))))

(define options
  ;; There are a different set of options for the crontab personality compared
  ;; to all the others, with the --help and --version options common to all
  ;; the personalities.
  (catch
   'misc-error
   (lambda ()
     (getopt-long (command-line)
                  (append
                   (case command-type
                     ((crontab)
                      '((user     (single-char #\u) (value #t))
                        (edit     (single-char #\e) (value #f))
                        (list     (single-char #\l) (value #f))
                        (remove   (single-char #\r) (value #f))))
                     (else `((schedule (single-char #\s) (value #t)
                                       (predicate
                                        ,(lambda (value)
                                           (string->number value))))
                             (daemon   (single-char #\d) (value #f))
                             (noetc    (single-char #\n) (value #f))
                             (stdin    (single-char #\i) (value #t)
                                       (predicate
                                        ,(lambda (value)
                                           (or (string=? "vixie" value)
                                               (string=? "guile" value))))))))
                   '((version  (single-char #\v) (value #f))
                     (help     (single-char #\h) (value #f))))))
   (lambda (key func fmt args . rest)
     (mcron-error 1 (apply format (append (list #f fmt) args))))))

(define* (show-version #:optional (command (command-name)))
  "Display version information for COMMAND and quit."
  (let* ((name       config-package-name)
         (short-name (cadr (string-split name #\space)))
         (version    config-package-version))
    (simple-format #t "~a (~a) ~a
Copyright (C) 2015 the ~a authors.
License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>
This is free software: you are free to change and redistribute it.
There is NO WARRANTY, to the extent permitted by law.\n"
		   command name version short-name)
    (quit)))

(define (show-package-information)
  "Display where to get help and send bug reports."
  (simple-format #t "\nReport bugs to: ~a.
~a home page: <~a>
General help using GNU software: <http://www.gnu.org/gethelp/>\n"
		 config-package-bugreport
		 config-package-name
		 config-package-url))

(define* (show-help #:optional (command (command-name)))
  "Display informations of usage for COMMAND and quit."
  (simple-format #t "Usage: ~a" command)
  (display
   (case command-type
     ((mcron)
      " [OPTIONS] [FILES]
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
     ((cron)
      " [OPTIONS]
Unless an option is specified, run a cron daemon as a detached process,
reading all the information in the users' crontabs and in /etc/crontab.

  -v, --version             Display version
  -h, --help                Display this help message
  -sN, --schedule[=]N       Display the next N jobs that will be run by cron
  -n, --noetc               Do not check /etc/crontab for updates (HIGHLY
                              RECOMMENDED).")
     ((crontab)
      " [-u user] file
       crontab [-u user] { -e | -l | -r }
               (default operation is replace, per 1003.2)
       -e      (edit user's crontab)
       -l      (list user's crontab)
       -r      (delete user's crontab")
     (else "\nrubbish")))
  (newline)
  (show-package-information)
  (quit))

(define (delete-run-file)
  "Remove the /var/run/cron.pid file so that crontab and other invocations of
cron don't get the wrong idea that a daemon is currently running.  This
procedure is called from the C front-end whenever a terminal signal is
received."
  (catch #t (lambda () (delete-file config-pid-file)
                       (delete-file config-socket-file))
            noop)
  (quit))

(define (stdin->string)
  "Return standard input as a string."
  (with-output-to-string (lambda () (do ((in (read-char) (read-char)))
                                        ((eof-object? in))
                                        (display in)))))

(define process-user-file
  (let ((guile-regexp (make-regexp "\\.gui(le)?$"))
        (vixie-regexp (make-regexp "\\.vix(ie)?$")))
    (lambda* (file-name #:optional guile-syntax?)
      "Process FILE-NAME according its extension.  When GUILE-SYNTAX? is TRUE,
force guile syntax usage.  If FILE-NAME format is not recognized, it is
silently ignored."
      (cond ((string=? "-" file-name)
             (if (string=? (option-ref options 'stdin "guile") "vixie")
                 (read-vixie-port (current-input-port))
                 (eval-string (stdin->string))))
            ((or guile-syntax? (regexp-exec guile-regexp file-name))
             (load file-name))
            ((regexp-exec vixie-regexp file-name)
             (read-vixie-file file-name))))))

(define (process-files-in-user-directory)
  "Process files in $XDG_CONFIG_HOME/cron and/or ~/.cron directories (if
$XDG_CONFIG_HOME is not defined uses ~/.config/cron instead)."
  (let ((errors 0)
        (home-directory (passwd:dir (getpw (getuid)))))
    (map (lambda (config-directory)
          (catch #t
                 (lambda ()
                   (let ((directory (opendir config-directory)))
                     (do ((file-name (readdir directory) (readdir directory)))
                         ((eof-object? file-name) (closedir directory))
                       (process-user-file (string-append config-directory
                                                         "/"
                                                         file-name)))))
                 (lambda (key . args)
                   (set! errors (1+ errors)))))
          (list (string-append home-directory "/.cron")
                (string-append (or (getenv "XDG_CONFIG_HOME")
                                   (string-append home-directory "/.config"))
                               "/cron")))
    (when (eq? 2 errors)
      (mcron-error 13
        "Cannot read files in your ~/.config/cron (or ~/.cron) directory."))))

(define (valid-user user-name)
  "Check that USER-NAME is in the passwd database (it may happen that a user
is removed after creating a crontab).  If the USER-NAME is valid, Return the
full passwd entry for that user."
  (setpwent)
  (do ((entry (getpw) (getpw)))
      ((or (not entry)
           (string=? (passwd:name entry) user-name))
       (endpwent)
       entry)))

(define (process-files-in-system-directory)
  "Process all the files in the crontab directory.  When the job procedure is
run on behalf of the configuration files, the jobs are registered on the
system with the appropriate user.  Only root should be able to perform this
operation.  The permissions on the /var/cron/tabs directory enforce this."
  (catch #t
         (lambda ()
           (let ((directory (opendir config-spool-dir)))
             (do ((file-name (readdir directory) (readdir directory)))
                 ((eof-object? file-name))
               (and-let* ((user (valid-user file-name)))
                         (set-configuration-user user)         ;; / ?? !!!!
                         (catch-mcron-error
                          (read-vixie-file (string-append config-spool-dir
                                                          "/"
                                                          file-name)))))))
         (lambda (key . args)
           (mcron-error 4
             "You do not have permission to access the system crontabs."))))

(define (cron-file-descriptors)
  "Establish a socket to listen for updates from a crontab program, and return
a list containing the file descriptors correponding to the files read by
crontab.  This requires that command-type is 'cron."
  (if (eq? command-type 'cron)
      (catch #t
	(lambda ()
	  (let ((sock (socket AF_UNIX SOCK_STREAM 0)))
	    (bind sock AF_UNIX config-socket-file)
	    (listen sock 5)
	    (list sock)))
	(lambda (key . args)
	  (delete-file config-pid-file)
	  (mcron-error 1 "Cannot bind to UNIX socket " config-socket-file)))
      '()))

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


;;;
;;; Entry point.
;;;

(define (main . args)
  ;; Turn debugging on if indicated.
  (when config-debug
    (debug-enable 'backtrace))
  (when (option-ref options 'version #f)
    (show-version))
  (when (option-ref options 'help #f)
    (show-help))

  ;; Setup the cron process, if appropriate. If there is already a
  ;; /var/run/cron.pid file, then we must assume a cron daemon is already
  ;; running and refuse to start another one.
  ;;
  ;; Otherwise, clear the MAILTO environment variable so that output from cron
  ;; jobs is sent to the various users (this may still be overridden in the
  ;; configuration files), and call the function in the C wrapper to set up
  ;; terminal signal responses to vector to the procedure above. The PID file
  ;; will be filled in properly later when we have forked our daemon process
  ;; (but not done if we are only viewing the schedules).
  (when (eq? command-type 'cron)
    (unless (eqv? (getuid) 0)
      (mcron-error 16
        "This program must be run by the root user (and should have been "
        "installed as such)."))
    (when (access? config-pid-file F_OK)
      (mcron-error 1
        "A cron daemon is already running.\n  (If you are sure this is not"
        " true, remove the file\n   " config-pid-file ".)"))
    (unless (option-ref options 'schedule #f)
      (with-output-to-file config-pid-file noop))
    (setenv "MAILTO" #f)
    ;; XXX: At compile time, this yields a "possibly unbound variable"
    ;; warning, but this is OK since it is bound in the C wrapper.
    (c-set-cron-signals))

  ;; Now we have the procedures in place for dealing with the contents of
  ;; configuration files, the crontab personality is able to validate such
  ;; files. If the user requested the crontab personality, we load and run the
  ;; code here and then get out.
  (when (eq? command-type 'crontab)
    (load "crontab.scm")
    (quit))

  ;; Having defined all the necessary procedures for scanning various sets of
  ;; files, we perform the actual configuration of the program depending on
  ;; the personality we are running as. If it is mcron, we either scan the
  ;; files passed on the command line, or else all the ones in the user's
  ;; .config/cron (or .cron) directory. If we are running under the cron
  ;; personality, we read the /var/cron/tabs directory and also the
  ;; /etc/crontab file.
  (case command-type
    ((mcron)
     (if (null? (option-ref options '() '()))
         (process-files-in-user-directory)
         (for-each (lambda (file-path) (process-user-file file-path #t))
                   (option-ref options '() '()))))
    ((cron)
     (process-files-in-system-directory)
     (use-system-job-list)
     (catch-mcron-error (read-vixie-file "/etc/crontab"
                                         parse-system-vixie-line))
     (use-user-job-list)
     (unless (option-ref options 'noetc #f)
       (display "\
WARNING: cron will check for updates to /etc/crontab EVERY MINUTE. If you do
not use this file, or you are prepared to manually restart cron whenever you
make a change, then it is HIGHLY RECOMMENDED that you use the --noetc
option.\n")
       (set-configuration-user "root")
       (job '(- (next-minute-from (next-minute)) 6)
            check-system-crontab
            "/etc/crontab update checker."))))

  ;; If the user has requested a schedule of jobs that will run, we provide
  ;; the information here and then get out.  Start by determining the number
  ;; of time points in the future that output is required for. This may be
  ;; provided on the command line as a parameter to the --schedule option, or
  ;; else we assume a default of 8. Finally, ensure that the count is some
  ;; positive integer.
  (and-let* ((count (option-ref options 'schedule #f)))
            (set! count (string->number count))
            (display (get-schedule (if (<= count 0) 1 count)))
            (quit))

  ;; If we are supposed to run as a daemon process (either a --daemon option
  ;; has been explicitly used, or we are running as cron or crond), detach
  ;; from the terminal now. If we are running as cron, we can now write the
  ;; PID file.
  (when (option-ref options 'daemon (eq? command-type 'cron))
    (unless (eqv? (primitive-fork) 0)
      (quit))
    (setsid)
    (when (eq? command-type 'cron)
      (with-output-to-file config-pid-file
        (lambda () (display (getpid)) (newline)))))

  ;; Now the main loop. Forever execute the run-job-loop procedure in the
  ;; mcron core, and when it drops out (can only be because a message has come
  ;; in on the socket) we process the socket request before restarting the
  ;; loop again.  Sergey Poznyakoff: we can also drop out of run-job-loop
  ;; because of a SIGCHLD, so must test FDES-LIST.
  (catch-mcron-error
   (let ((fdes-list (cron-file-descriptors)))
     (while #t
       (run-job-loop fdes-list)
       (unless (null? fdes-list)
         (process-update-request fdes-list))))))
