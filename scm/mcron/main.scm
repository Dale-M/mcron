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



;; This is the 'main' routine for the whole system; the top of this file is the
;; global entry point (after the minimal C wrapper, mcron.c.template); to all
;; intents and purposes the program is pure Guile and starts here.
;;
;; This file is built into mcron.c.template by the makefile, which stringifies
;; the whole lot, and escapes quotation marks and escape characters
;; accordingly. Bear this in mind when considering literal multi-line strings.
;;
;; (l0ad "crontab.scm") (sic) is inlined by the makefile. All other
;; functionality comes through modules in .../share/guile/site/mcron/*.scm.



;; Pull in some constants set by the builder (via autoconf) at configuration
;; time. Turn debugging on if indicated.

(use-modules (mcron config))
(if config-debug (begin (debug-enable 'debug)
                        (debug-enable 'backtrace)))



;; To determine the name of the program, scan the first item of the command line
;; backwards for the first non-alphabetic character. This allows names like
;; in.cron to be accepted as an invocation of the cron command.

(use-modules (ice-9 regex) (ice-9 rdelim))

(define command-name (match:substring (regexp-exec (make-regexp "[[:alpha:]]*$")
                                                   (car (command-line)))))



;; Code contributed by Sergey Poznyakoff.  Print an error message (made up from
;; the parts of rest), and if the error is fatal (present and non-zero) then
;; exit to the system with this code.

(define (mcron-error exit-code . rest)
  (with-output-to-port (current-error-port)
    (lambda ()
      (for-each display (append (list command-name ": ") rest))
      (newline)))
  (if (and exit-code (not (eq? exit-code 0)))
      (primitive-exit exit-code)))



;; Code contributed by Sergey Poznyakoff.  Execute body. If an 'mcron-error
;; exception occurs, print its diagnostics and exit with its error code.

(defmacro catch-mcron-error (. body)
  `(catch 'mcron-error
          (lambda ()
            ,@body)
          (lambda (key exit-code . msg)
            (apply mcron-error exit-code msg))))



;; We will be doing a lot of testing of the command name, so it makes sense to
;; perform the string comparisons once and for all here.

(define command-type (cond ((string=? command-name "mcron") 'mcron)
                           ((or (string=? command-name "cron")
                                (string=? command-name "crond")) 'cron)
                           ((string=? command-name "crontab") 'crontab)
                           (else
                            (mcron-error 12 "The command name is invalid."))))



;; There are a different set of options for the crontab personality compared to
;; all the others, with the --help and --version options common to all the
;; personalities.

(use-modules (ice-9 getopt-long))

(define options
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

;; If the user asked for the version of this program, give it to him and get
;; out.

(if (option-ref options 'version #f)
    (begin
      (display (string-append "\n
" command-name "  (" config-package-string ")\n
Written by Dale Mellor\n
\n
Copyright (C) 2003, 2006, 2014  Dale Mellor\n
This is free software; see the source for copying conditions.  There is NO\n
warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.\n
"))
      (quit)))



;; Likewise if the user requested the help text.

(if (option-ref options 'help #f)
    (begin
      (display (string-append "
Usage: " (car (command-line))
(case command-type

  ((mcron)
" [OPTIONS] [FILES]\n
Run an mcron process according to the specifications in the FILES (`-' for\n
standard input), or use all the files in ~/.config/cron (or the \n
deprecated ~/.cron) with .guile or .vixie extensions.\n
\n
  -v, --version             Display version\n
  -h, --help                Display this help message\n
  -sN, --schedule[=]N       Display the next N jobs that will be run by mcron\n
  -d, --daemon              Immediately detach the program from the terminal\n
                              and run as a daemon process\n
  -i, --stdin=(guile|vixie) Format of data passed as standard input or\n
                              file arguments (default guile)")

  ((cron)
" [OPTIONS]\n
Unless an option is specified, run a cron daemon as a detached process, \n
reading all the information in the users' crontabs and in /etc/crontab.\n
\n
  -v, --version             Display version\n
  -h, --help                Display this help message\n
  -sN, --schedule[=]N       Display the next N jobs that will be run by cron\n
  -n, --noetc               Do not check /etc/crontab for updates (HIGHLY\n
                              RECOMMENDED).")
  
  ((crontab)
           (string-append " [-u user] file\n"
           "       " (car (command-line)) " [-u user] { -e | -l | -r }\n"
           "               (default operation is replace, per 1003.2)\n"
           "       -e      (edit user's crontab)\n"
           "       -l      (list user's crontab)\n"
           "       -r      (delete user's crontab)\n"))

  (else "rubbish"))

"\n\n
Report bugs to " config-package-bugreport ".\n
"))
      (quit)))



;; This is called from the C front-end whenever a terminal signal is
;; received. We remove the /var/run/cron.pid file so that crontab and other
;; invocations of cron don't get the wrong idea that a daemon is currently
;; running.

(define (delete-run-file)
  (catch #t (lambda () (delete-file config-pid-file)
                       (delete-file config-socket-file))
            noop)
  (quit))



;; Setup the cron process, if appropriate. If there is already a
;; /var/run/cron.pid file, then we must assume a cron daemon is already running
;; and refuse to start another one.
;;
;; Otherwise, clear the MAILTO environment variable so that output from cron
;; jobs is sent to the various users (this may still be overridden in the
;; configuration files), and call the function in the C wrapper to set up
;; terminal signal responses to vector to the procedure above. The PID file will
;; be filled in properly later when we have forked our daemon process (but not
;; done if we are only viewing the schedules).

(if (eq? command-type 'cron)
    (begin
      (if (not (eqv? (getuid) 0))
          (mcron-error 16
                       "This program must be run by the root user (and should "
                       "have been installed as such)."))
      (if (access? config-pid-file F_OK)
          (mcron-error 1
		       "A cron daemon is already running.\n"
		       "  (If you are sure this is not true, remove the file\n"
		       "   "
		       config-pid-file
		       ".)"))
      (if (not (option-ref options 'schedule #f))
          (with-output-to-file config-pid-file noop))
      (setenv "MAILTO" #f)
      (c-set-cron-signals)))



;; Define the functions available to the configuration files. While we're here,
;; we'll get the core loaded as well.

(use-modules (mcron core)
             (mcron job-specifier)
             (mcron vixie-specification))



;; Procedure to slurp the standard input into a string.

(define (stdin->string)
  (with-output-to-string (lambda () (do ((in (read-char) (read-char)))
                                        ((eof-object? in))
                                        (display in)))))



;; Now we have the procedures in place for dealing with the contents of
;; configuration files, the crontab personality is able to validate such
;; files. If the user requested the crontab personality, we load and run the
;; code here and then get out.

(if (eq? command-type 'crontab)
    (begin
      (load "crontab.scm")
      (quit)))



;; Code contributed by Sergey Poznyakoff.  Determine if the given file is a
;; regular file or not.

(define (regular-file? file)
  (catch 'system-error
	 (lambda ()
	   (eq? (stat:type (stat file)) 'regular))
	 (lambda (key call fmt args . rest)
	   (mcron-error 0 (apply format (append (list #f fmt) args)))
	   #f)))



;; Procedure which processes any configuration file according to the
;; extension. If a file is not recognized, it is silently ignored (this deals
;; properly with most editors' backup files, for instance).

(define guile-file-regexp (make-regexp "\\.gui(le)?$"))
(define vixie-file-regexp (make-regexp "\\.vix(ie)?$"))

(define (process-user-file file-path . assume-guile)
  (cond ((string=? file-path "-")
               (if (string=? (option-ref options 'stdin "guile") "vixie")
                   (read-vixie-port (current-input-port))
                   (eval-string (stdin->string))))
        ((or (not (null? assume-guile))
             (regexp-exec guile-file-regexp file-path))
         (load file-path))
        ((regexp-exec vixie-file-regexp file-path)
         (read-vixie-file file-path))))



;; Procedure to run through all the files in a user's ~/.cron and/or
;; $XDG_CONFIG_HOME/cron or ~/.config/cron directories (only happens under the
;; mcron personality).

(define (process-files-in-user-directory)
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
    (if (eq? 2 errors)
        (mcron-error 13
                     "Cannot read files in your ~/.config/cron (or ~/.cron) "
                     "directory."))))



;; Procedure to check that a user name is in the passwd database (it may happen
;; that a user is removed after creating a crontab). If the user name is valid,
;; the full passwd entry for that user is returned to the caller.

(define (valid-user user-name)
  (setpwent)
  (do ((entry (getpw) (getpw)))
      ((or (not entry)
           (string=? (passwd:name entry) user-name))
       (endpwent)
       entry)))



;; Procedure to process all the files in the crontab directory, making sure that
;; each file is for a legitimate user and setting the configuration-user to that
;; user. In this way, when the job procedure is run on behalf of the
;; configuration files, the jobs are registered with the system with the
;; appropriate user. Note that only the root user should be able to perform this
;; operation, but we leave it to the permissions on the /var/cron/tabs directory
;; to enforce this.

(use-modules (srfi srfi-2))  ;; For and-let*.

(define (process-files-in-system-directory)
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
           (mcron-error
            4
            "You do not have permission to access the system crontabs."))))



;; Having defined all the necessary procedures for scanning various sets of
;; files, we perform the actual configuration of the program depending on the
;; personality we are running as. If it is mcron, we either scan the files
;; passed on the command line, or else all the ones in the user's .config/cron
;; (or .cron) directory. If we are running under the cron personality, we read
;; the /var/cron/tabs directory and also the /etc/crontab file.

(case command-type
  ((mcron) (if (null? (option-ref options '() '()))
                (process-files-in-user-directory)
                (for-each (lambda (file-path)
                            (process-user-file file-path #t))
                          (option-ref options '() '()))))
  
  ((cron) (process-files-in-system-directory)
   (use-system-job-list)
   (catch-mcron-error
    (read-vixie-file "/etc/crontab" parse-system-vixie-line))
   (use-user-job-list)
   (if (not (option-ref options 'noetc #f))
       (begin
         (display
"WARNING: cron will check for updates to /etc/crontab EVERY MINUTE. If you do\n
not use this file, or you are prepared to manually restart cron whenever you\n
make a change, then it is HIGHLY RECOMMENDED that you use the --noetc\n
option.\n")
         (set-configuration-user "root")
         (job '(- (next-minute-from (next-minute)) 6)
              check-system-crontab
              "/etc/crontab update checker.")))))



;; If the user has requested a schedule of jobs that will run, we provide the
;; information here and then get out.
;;
;; Start by determining the number of time points in the future that output is
;; required for. This may be provided on the command line as a parameter to the
;; --schedule option, or else we assume a default of 8. Finally, ensure that the
;; count is some positive integer.

(and-let* ((count (option-ref options 'schedule #f)))
          (set! count (string->number count))
          (display (get-schedule (if (<= count 0) 1 count)))
          (quit))
    


;; If we are supposed to run as a daemon process (either a --daemon option has
;; been explicitly used, or we are running as cron or crond), detach from the
;; terminal now. If we are running as cron, we can now write the PID file.

(if (option-ref options 'daemon (eq? command-type 'cron))
    (begin
      (if (not (eqv? (primitive-fork) 0))
          (quit))
      (setsid)
      (if (eq? command-type 'cron)
          (with-output-to-file config-pid-file
            (lambda () (display (getpid)) (newline))))))



;; If we are running as cron or crond, we establish a socket to listen for
;; updates from a crontab program. This is put into fd-list so that we can
;; inform the main wait-run-wait execution loop to listen for incoming messages
;; on this socket.

(define fd-list '())

(if (eq? command-type 'cron)
    (catch #t
           (lambda ()
             (let ((socket (socket AF_UNIX SOCK_STREAM 0)))
               (bind socket AF_UNIX config-socket-file)
               (listen socket 5)
               (set! fd-list (list socket))))
           (lambda (key . args)
             (delete-file config-pid-file)
             (mcron-error 1
                          "Cannot bind to UNIX socket "
                          config-socket-file))))
		     



;; This function is called whenever a message comes in on the above socket. We
;; read a user name from the socket, dealing with the "/etc/crontab" special
;; case, remove all the user's jobs from the job list, and then re-read the
;; user's updated file. In the special case we drop all the system jobs and
;; re-read the /etc/crontab file.

(define (process-update-request)
  (let* ((socket (car (accept (car fd-list))))
         (user-name (read-line socket)))
    (close socket)
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



;; Added by Sergey Poznyakoff.  This no-op will collect zombie child processes
;; as soon as they die.  This is a big improvement as previously they stayed
;; around the system until the next time mcron wakes to fire a new job off.

;; Unfortunately it seems to interact badly with the select system call,
;; wreaking havoc...

;; (sigaction SIGCHLD (lambda (sig) noop) SA_RESTART)



;; Now the main loop. Forever execute the run-job-loop procedure in the mcron
;; core, and when it drops out (can only be because a message has come in on the
;; socket) we process the socket request before restarting the loop again.
;; Sergey Poznyakoff: we can also drop out of run-job-loop because of a SIGCHLD,
;; so must test fd-list.

(catch-mcron-error
 (while #t
        (run-job-loop fd-list)
        (if (not (null? fd-list))
            (process-update-request))))
