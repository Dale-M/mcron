;;   Copyright (C) 2003 Dale Mellor
;; 
;;   This program is free software; you can redistribute it and/or modify
;;   it under the terms of the GNU General Public License as published by
;;   the Free Software Foundation; either version 2, or (at your option)
;;   any later version.
;; 
;;   This program is distributed in the hope that it will be useful,
;;   but WITHOUT ANY WARRANTY; without even the implied warranty of
;;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;   GNU General Public License for more details.
;; 
;;   You should have received a copy of the GNU General Public License
;;   along with this program; if not, write to the Free Software
;;   Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
;;   USA.



;; This is the 'main' routine for the whole system; the top of this file is the
;; global entry point (after the minimal C wrapper, mcron.c.template). To all
;; intents and purposes the program is pure Guile and starts here.
;;
;; This file is built into mcron.c.template by the makefile, which stringifies
;; the whole lot, and escapes quotation marks and escape characters
;; accordingly. Bear this in mind when considering literal multi-line strings.
;;
;; (load ...)'s are inlined by the makefile.


;; Make a note of the time the script started; regardless of how long it takes
;; to initialize things, we will run any job scheduled to run after this exact
;; second.

(define configuration-time (current-time))



;; Pull in some constants set by the builder (via autoconf) at configuration
;; time. Turn debugging on if indicated.

(load "config.scm")
(if config-debug (begin (debug-enable 'debug)
                        (debug-enable 'backtrace)))



;; To determine the name of the program, scan the first item of the command line
;; backwards for the first non-alphabetic character. This allows names like
;; in.cron to be accepted as an invocation of the cron command.

(use-modules (ice-9 regex))

(define command-name (match:substring (regexp-exec (make-regexp "[[:alpha:]]*$")
                                                   (car (command-line)))))



;; We will be doing a lot of testing of the command name, so it makes sense to
;; perform the string comparisons once and for all here.

(define command-type (cond ((string=? command-name "mcron") 'mcron)
                           ((or (string=? command-name "cron")
                                (string=? command-name "crond")) 'cron)
                           ((string=? command-name "crontab") 'crontab)
                           (else
                            (display "The command name is invalid.\n")
                            (primitive-exit 12))))



;; There are a different set of options for the crontab personality compared to
;; all the others, with the --help and --version options common to all the
;; personalities.

(use-modules (ice-9 getopt-long))

(define options
  (getopt-long (command-line)
               (append
                (case command-type ('crontab
                            '((user    (single-char #\u) (value #t))
                              (edit    (single-char #\e) (value #f))
                              (list    (single-char #\l) (value #f))
                              (remove  (single-char #\r) (value #f))))
                      (else `((schedule (single-char #\s) (value optional))
                              (daemon   (single-char #\d) (value #f))
                              (stdin    (single-char #\i) (value #t)
                                        (predicate
                                         ,(lambda (value)
                                           (or (string=? "vixie" value)
                                               (string=? "guile" value))))))))
                '((version  (single-char #\v) (value #f))
                  (help     (single-char #\h) (value #f))))))




;; If the user asked for the version of this program, give it to him and get
;; out.

(if (option-ref options 'version #f)
    (begin
      (display (string-append "\n
" command-name "  (" config-package-string ")\n
Written by Dale Mellor\n
\n
Copyright (C) 2003  Dale Mellor\n
This is free software; see the source for copying conditions.  There is NO\n
warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.\n
"))
      (quit)))



;; Likewise if the user requested the help text.

(if (option-ref options 'help #f)
    (begin
      (display (string-append "
Usage: " (car (command-line))
(case command-type ('mcron
" [OPTIONS] [FILES]\n
Run an mcron process according to the specifications in the FILES (`-' for\n
standard input), or use all the files in ~/.cron with .guile or .vixie\n
extensions.\n
\n
  -v, --version             Display version\n
  -h, --help                Display this help message\n
  -s, --schedule[=COUNT]    Display the next COUNT jobs (default 8) that\n
                              will be run by mcron\n
  -d, --daemon              Immediately detach the program from the terminal and\n
                              run as a daemon process\n
  -i, --stdin=(guile|vixie) Format of data passed as standard input\n
                              (default guile)")

  ('cron
" [OPTIONS]\n
Unless an option is specified, run a cron daemon as a detached process, \n
reading all the information in the users' crontabs and in /etc/crontab.\n
\n
  -v, --version             Display version\n
  -h, --help                Display this help message\n
  -s, --schedule[=COUNT]    Display the next COUNT jobs (default 8) that\n
                              will be run by cron")
  
  ('crontab
           (string-append " [-u user] file\n"
           "       " (car (command-line)) " [-u user] { -e | -l | -r }\n"
           "               (default operation is replace, per 1003.2)\n"
           "       -e      (edit user's crontab)\n"
           "       -l      (list user's crontab)\n"
           "       -r      (delete user's crontab)\n")))

"\n\n
Report bugs to " config-package-bugreport ".\n
"))
      (quit)))


;;----------------------------------------------------------------------
;;    Perform setup processing specific to cron, crond personalities.
;;----------------------------------------------------------------------

;; This is called from the C front-end whenever a terminal signal is
;; received. We simply remove the /var/run/cron.pid file so that crontab and
;; other invokations of cron don't get the wrong idea that a daemon is currently
;; running.

(define (delete-run-file)
  (catch #t (lambda () (delete-file "/var/run/cron.pid"))
            (lambda (key . args) #t))
  (quit))



;; Every time a SIGHUP is received from a crontab process, we read the
;; /var/cron/update file for a user name (he whose crontab has been modified)
;; and add it to this list (thus it may be regarded as a deferred update list).

(define hup-received-for '())



;; Two arbiters to control access to the above list. When an interrupt is
;; received, the list will only be modified if pending-lock is available. If it
;; is not, then the interrupt routine will lock interrupt-required and return
;; immediately to the system, which should at convenient times check this lock
;; and send a SIGHUP to the process to re-run the interrupt routine (obviously,
;; if the main program locks pending-lock (or leaves locked) and issues an
;; interrupt the interrupt routine will be a no-op).

(define pending-lock (make-arbiter "pending-lock"))
(define interrupt-required (make-arbiter "interrupt-required"))



;; This is called from the C front-end whenever a HUP signal is received. We
;; read the name of the user whose crontab has been modified, add his name to
;; the list of pending requests, and remove the update file as an
;; acknowledgement that we received the signal.
;;
;; ! We should put a warning in a log file if we receive a HUP and the update
;; file is not present.

(define (process-hup)
  (if (try-arbiter pending-lock)
      (begin
        (with-input-from-file "/var/cron/update" (lambda ()
          (set! hup-received-for (append hup-received-for (list (read-line))))))
        (delete-file "/var/cron/update")
        (release-arbiter pending-lock))
      (try-arbiter interrupt-required)))



;; Setup the cron process, if appropriate. If there is already a
;; /var/run/cron.pid file, then we must assume a cron daemon is already running
;; and refuse to start another one.
;;
;; Otherwise, clear the MAILTO environment variable so that output from cron
;; jobs is sent to the various users (this may still be overridden in the
;; configuration files), and call the function in the C wrapper to set up
;; terminal and hangup signal responses to vector to the two procedures
;; above. The PID file will be filled in properly later when we have forked our
;; daemon process (but not done if we are only viewing the schedules).

(if (eq? command-type 'cron)
    (begin
      (if (not (eqv? (getuid) 0))
          (begin
            (display "This program must be run by the root user (and should ")
            (display "have been installed as such).\n")
            (primitive-exit 16)))
      (if (access? "/var/run/cron.pid" F_OK)
          (begin
            (display "A cron daemon is already running.\n")
            (display "  (If you are sure this is not true, remove the file\n")
            (display "   /var/run/cron.pid.)\n")
            (primitive-exit 1)))
      (if (not (option-ref options 'schedule #f))
          (with-output-to-file "/var/run/cron.pid" noop))
      (setenv "MAILTO" #f)
      (c-set-cron-signals)))



;;----------------------------------------------------------------------
;;     Define the functions available to the configuration files.
;;----------------------------------------------------------------------


;; Define the with-mail-out command for configuration files to use (directly or
;; indirectly as is the case when we parse vixie-style files).

(load "email.scm")
  


;; Function (available to user configuration files) which produces a list of
;; values from start up to (but not including) end. An optional step may be
;; supplied, and (if positive) only every step'th value will go into the
;; list. For example, (range 1 6 2) returns '(1 3 5).

(define (range start end . step)
  (let ((step (if (or (null? step)
                      (<= (car step) 0))
                  1
                  (car step))))
    (let loop ((start start))
      (if (>= start end) '()
          (cons start
                (loop (+ start step)))))))



;; Internal function (not supposed to be used directly in configuration files)
;; which takes a value and a list of possible next values (all assumed less than
;; 9999). It returns a pair consisting of the smallest element of the list, and
;; the smallest element larger than the current value. If an example of the
;; latter cannot be found, 9999 will be returned.

(define (find-best-next current next-list)
  (let ((current-best (cons 9999 9999)))
    (for-each (lambda (allowed-time)
                          (if (< allowed-time (car current-best))
                              (set-car! current-best allowed-time))
                          (if (and (> allowed-time current)
                                   (< allowed-time (cdr current-best)))
                              (set-cdr! current-best allowed-time)))
              next-list)
    current-best))



;; Internal function to return the time corresponding to some near future
;; hour. If hour-list is not supplied, the time returned corresponds to the
;; start of the next hour of the day.
;;
;; If the hour-list is supplied the time returned corresponds to the first hour
;; of the day in the future which is contained in the list. If all the values in
;; the list are less than the current hour, then the time returned will
;; correspond to the first hour in the list *on the following day*.
;;
;; ... except that the function is actually generalized to deal with seconds,
;; minutes, etc., in an obvious way :-)
;;
;; Note that value-list always comes from an optional argument to a procedure,
;; so is wrapped up as the first element of a list (i.e. it is a list inside a
;; list).

(define (bump-time time value-list component higher-component
                   set-component! set-higher-component!)
  (if (null? value-list)
      (set-component! time (+ (component time) 1))
      (let ((best-next (find-best-next (component time) (car value-list))))
        (if (eqv? 9999 (cdr best-next))
            (begin
              (set-higher-component! time (+ (higher-component time) 1))
              (set-component! time (car best-next)))
            (set-component! time (cdr best-next)))))
  (car (mktime time)))




;; Set of configuration methods which use the above general function to bump
;; specific components of time to the next legitimate value. In each case, all
;; the components smaller than that of interest are taken to zero, so that for
;; example the time of the next year will be the time at which the next year
;; actually starts.

(define (next-year-from current-time . year-list)
  (let ((time (localtime current-time)))
    (set-tm:mon   time 0)
    (set-tm:mday  time 1)
    (set-tm:hour  time 0)
    (set-tm:min   time 0)
    (set-tm:sec   time 0)
    (bump-time time year-list tm:year tm:year set-tm:year set-tm:year)))

(define (next-month-from current-time . month-list)
  (let ((time (localtime current-time)))
    (set-tm:mday  time 1)
    (set-tm:hour  time 0)
    (set-tm:min   time 0)
    (set-tm:sec   time 0)
    (bump-time time month-list tm:mon tm:year set-tm:mon set-tm:year)))

(define (next-day-from current-time . day-list)
  (let ((time (localtime current-time)))
    (set-tm:hour  time 0)
    (set-tm:min   time 0)
    (set-tm:sec   time 0)
    (bump-time time day-list tm:mday tm:mon set-tm:mday set-tm:mon)))

(define (next-hour-from current-time . hour-list)
  (let ((time (localtime current-time)))
    (set-tm:min   time 0)
    (set-tm:sec   time 0)
    (bump-time time hour-list tm:hour tm:mday set-tm:hour set-tm:mday)))

(define (next-minute-from current-time . minute-list)
  (let ((time (localtime current-time)))
    (set-tm:sec   time 0)
    (bump-time time minute-list tm:min tm:hour set-tm:min set-tm:hour)))

(define (next-second-from current-time . second-list)
  (let ((time (localtime current-time)))
    (bump-time time second-list tm:sec tm:min set-tm:sec set-tm:min)))



;; The current-action-time is the time a job was last run, the time from which
;; the next time to run a job must be computed. (When the program is first run,
;; this time is set to the configuration time so that jobs run from that moment
;; forwards.) Once we have this, we supply versions of the time computation
;; commands above which implicitly assume this value.

(define current-action-time configuration-time)



;; We want to provide functions which take a single optional argument (as well
;; as implicitly the current action time), but unlike usual scheme behaviour if
;; the argument is missing we want to act like it is really missing, and if it
;; is there we want to act like it is a genuine argument, not a list of
;; optionals.

(define (maybe-args function args)
  (if (null? args)
      (function current-action-time)
      (function current-action-time (car args))))



;; These are the convenience functions we were striving to define for the
;; configuration files. They are wrappers for the next-X-from functions above,
;; but implicitly use the current-action-time for the time argument.

(define (next-year   . args) (maybe-args next-year-from args))
(define (next-month  . args) (maybe-args next-month-from args))
(define (next-day    . args) (maybe-args next-day-from args))
(define (next-hour   . args) (maybe-args next-hour-from args))
(define (next-minute . args) (maybe-args next-minute-from args))
(define (next-second . args) (maybe-args next-second-from args))



;; The list of all jobs known to the system. Each element of the list is
;;
;;  (vector user next-time-function action environment next-time)
;;
;; where action may be a string (indicating a shell command) or a list
;; (indicating scheme code) or a procedure, and the environment is an alist of
;; modifications that need making to the UNIX environment before the action is
;; run. The next-time elements is the only one that is modified during the
;; running of a cron process (i.e. all the others are set once and for all at
;; configuration time).

(define job-list '())



;; Convenience functions for getting and setting the elements of a job object.

(define (job:user job)                (vector-ref job 0))
(define (job:next-time-function job)  (vector-ref job 1))
(define (job:action job)              (vector-ref job 2))
(define (job:environment job)         (vector-ref job 3))
(define (job:next-time job)           (vector-ref job 4))
(define (job:set-next-time! job time) (vector-set! job 4 time))



;; Introduce the definition of an environment object, and provide methods for
;; its manipulation and application to the environment in which we run a job.

(load "environment.scm")



;; Introduce functions which can be used directly in configuration files or
;; indirectly to parse vixie-style time specification strings and manufacture
;; corresponding next-time functions like the ones above.

(load "vixie.scm")



;; The default user for running jobs is the current one (who invoked this
;; program). There are exceptions: when cron parses /etc/crontab the user is
;; specified on each individual line; when cron parses /var/cron/tabs/* the user
;; is derived from the filename of the crontab. These cases are dealt with by
;; mutating this variable. Note that the variable is only used at configuration
;; time; a UID is stored with each job and it is that which takes effect when
;; the job actually runs.

(define configuration-user (getpw (getuid)))



;; The job function, available to configuration files for adding a job rule to
;; the system.
;;
;; Here we must 'normalize' the next-time-function so that it is always a lambda
;; function which takes one argument (the last time the job ran) and returns a
;; single value (the next time the job should run). If the input value is a
;; string this is parsed as a Vixie-style time specification, and if it is a
;; list then we arrange to eval it (but note that such lists are expected to
;; ignore the function parameter - the last run time is always read from the
;; current-action-time global variable). A similar normalization is applied to
;; the action.
;;
;; Here we also compute the first time that the job is supposed to run, by
;; finding the next legitimate time from the current configuration time (set
;; right at the top of this program).
;;
;; Note that the new job is added at the front of the job-list (this is
;; important so that the entries in the system crontab /etc/crontab finish up at
;; the front of the list when we scan that file).

(define (job time-proc action)
  (let ((action (cond ((procedure? action) action)
                      ((list? action) (lambda () (primitive-eval action)))
                      ((string? action) (lambda () (system action)))
                      (else 
           (display "job: invalid second argument (action; should be lamdba")
           (display "function, string or list)\n")
                         (primitive-exit 2))))

        (time-proc
         (cond ((procedure? time-proc) time-proc)
               ((string? time-proc)    (parse-vixie-time time-proc))
               ((list? time-proc)      (lambda (dummy)
                                         (primitive-eval time-proc)))
               (else

          (display "job: invalid first argument (next-time-function; should ")
          (display "be function, string or list)")
                    (primitive-exit 3)))))

    (set! job-list (cons (vector configuration-user
                                 time-proc
                                 action
                                 (list-copy current-environment-mods)
                                 (time-proc current-action-time))
                         job-list))))


;;----------------------------------------------------------------------
;;   End of definition of procedures for configuration files.
;;----------------------------------------------------------------------



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



;;----------------------------------------------------------------------
;;   Procedures for effecting the configuration process itself.
;;----------------------------------------------------------------------


;; Procedure which processes any configuration file according to the
;; extension. If a file is not recognized, it is silently ignored (this deals
;; properly with most editors' backup files, for instance).

(define guile-file-regexp (make-regexp "\\.gui(le)?$"))
(define vixie-file-regexp (make-regexp "\\.vix(ie)?$"))

(define (process-user-file file-path)
  (cond ((string=? file-path "-")
               (if (string=? (option-ref options 'stdin "guile") "vixie")
                   (read-vixie-port (current-input-port))
                   (eval-string (stdin->string))))
        ((regexp-exec guile-file-regexp file-path)
               (load file-path))
        ((regexp-exec vixie-file-regexp file-path)
               (read-vixie-file file-path))))



;; Procedure to run through all the files in a user's ~/.cron directory (only
;; happens under the mcron personality).

(define (process-files-in-user-directory)
  (catch #t (lambda ()
              (let* ((dir-path (string-append (passwd:dir configuration-user)
                                              "/.cron"))
                     (directory (opendir dir-path)))
                (do ((file-name (readdir directory) (readdir directory)))
                    ((eof-object? file-name) (closedir directory))
                    (process-user-file (string-append dir-path
                                                      "/"
                                                      file-name)))))
         (lambda (key . args)
           (display "Cannot read files in your ~/.cron directory.\n")
           (primitive-exit 13))))



;; Procedure to check that a user name is the the passwd database (it may happen
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

(use-modules (srfi srfi-2))

(define (process-files-in-system-directory)
  (catch #t (lambda ()
              (let ((directory (opendir "/var/cron/tabs")))
                (do ((file-name (readdir directory) (readdir directory)))
                    ((eof-object? file-name) (closedir directory))
                  (and-let* ((user (valid-user file-name)))
                            (set! configuration-user user)
                            (read-vixie-file (string-append "/var/cron/tabs/"
                                                            file-name))))))
      (lambda (key . args)
        (display "You do not have permission to access the system crontabs.\n")
        (primitive-exit 4))))



;; The head of the jobs list will contain the jobs specified in /etc/crontab,
;; and this variable tells us how long that head is.

(define system-jobs 0)



;; Having defined all the necessary procedures for scanning various sets of
;; files, we perform the actual configuration of the program depending on the
;; personality we are running as. If it is mcron, we either scan the files
;; passed on the command line, or else all the ones in the user's .cron
;; directory. If we are running under the cron personality, we read the
;; /var/cron/tabs directory and also the /etc/crontab file.

(case command-type
  ('mcron (if (null? (option-ref options '() '()))
              (process-files-in-user-directory)
              (for-each (lambda (file-path)
                          (process-user-file file-path))
                        (option-ref options '() '()))))
 
  ('cron (process-files-in-system-directory)
         (let ((start-length (length job-list)))
           (read-vixie-file "/etc/crontab" parse-system-vixie-line)
           (set! system-jobs (- (length job-list) start-length)))))
  


;;----------------------------------------------------------------------
;;   End of configuration section.
;;
;;   Now the main execution loop.
;;----------------------------------------------------------------------



;; Procedure to locate the jobs in the global job-list with the lowest
;; (soonest) next-times. These are the jobs for which we must schedule the mcron
;; program (under any personality) to next wake up. The return value is a cons
;; cell consisting of the next time (maintained in the next-time variable) and a
;; list of the job entries that are to run at this time (maintained in the
;; next-jobs-list variable).
;;
;; The procedure works by first obtaining the time of the first job on the list,
;; and setting this job in the next-jobs-list. Then for each other entry on the
;; job-list, either the job runs earlier than any other that have been scanned,
;; in which case the next-time and next-jobs-list are re-initialized to
;; accomodate, or the job runs at the same time as the next job, in which case
;; the next-jobs-list is simply augmented with the new job, or else the job runs
;; later than others noted in which case we ignore it for now and continue to
;; recurse the list.

(define (find-next-jobs)

  (if (null? job-list)
      (if (eq? command-type 'mcron)
          (begin (display "Nothing to do.\n")
                 (primitive-exit 5))
          (cons #f '()))
  
      (let ((next-time (job:next-time (car job-list)))
            (next-jobs-list (list (car job-list))))

        (for-each
            (lambda (job)
                 (let ((this-time (job:next-time job)))
                   (cond ((< this-time next-time)
                             (set! next-time this-time)
                             (set! next-jobs-list (list job)))
                         ((eqv? this-time next-time)
                             (set! next-jobs-list (cons job next-jobs-list))))))
         (cdr job-list))

        (cons next-time next-jobs-list))))



;; If the user has requested a schedule of jobs that will run, we provide the
;; information here and then get out.
;;
;; Start by determining the number of time points in the future that output is
;; required for. This may be provided on the command line as a parameter to the
;; --schedule option, or else we assume a default of 8. Having determined this
;; count we enter a loop of displaying the next set of jobs to run, artificially
;; forwarding the time to the next time point (instead of waiting for it to
;; occur as we would do in a normal run of mcron), and recurse around the loop
;; count times.

(and-let* ((count (option-ref options 'schedule #f)))
          (set! count (if (eq? count #t)
                          8
                          (string->number count)))
          (if (<= count 0) (set! count 1))
          (do ((count count (- count 1)))
              ((eqv? count 0))
            (let* ((next-jobs (find-next-jobs))
                   (date-string (strftime "%c\n" (localtime (car next-jobs)))))
              (for-each (lambda (job) (display date-string)
                                      (write (job:action job))
                                      (newline)(newline))
                        (cdr next-jobs))))
          (quit))
    


;; For proper housekeeping, it is necessary to keep a record of the number of
;; child processes we fork off to run the jobs.

(define number-children 0)



;; For every job on the list, fork a process to run it (noting the fact by
;; increasing the number-children counter), and in the new process set up the
;; run-time environment exactly as it should be before running the job proper.
;;
;; In the parent, update the job entry by computing the next time the job needs
;; to run.

(define (run-jobs jobs-list)
  (for-each (lambda (job)
              (if (eqv? (primitive-fork) 0)
                  (begin
                    (setuid (passwd:uid (job:user job)))
                    (chdir (passwd:dir (job:user job)))
                    (modify-environment (job:environment job) (job:user job))
                    ((job:action job))
                    (primitive-exit 0))
                  (begin
                    (set! number-children (+ number-children 1))
                    (set! current-action-time (job:next-time job))
                    (job:set-next-time! job
                                        ((job:next-time-function job)
                                                       current-action-time)))))
            jobs-list))



;; If we are supposed to run as a daemon process (either a --daemon option has
;; been explicitly used, or we are running as cron or crond), detach from the
;; terminal now. If we are running as cron, we can now write the PID file.

(if (option-ref options 'daemon (eq? command-type 'cron))
    (begin
      (if (not (eqv? (primitive-fork) 0))
          (quit))
      (setsid)
      (if (eq? command-type 'cron)
          (with-output-to-file "/var/run/cron.pid"
            (lambda () (display (getpid)) (newline))))))



;; Now the main loop. Take the current time. Loop over all job specifications,
;; get a list of the next ones to run (may be more than one). Set an alarm and
;; go to sleep. When we wake, run the jobs. Repeat ad infinitum.

(use-modules (srfi srfi-1))

(let main-loop ()

  (release-arbiter pending-lock)
  
  ;; Check for any pending updates to the configuration files (as notified by
  ;; crontab). If one is seen, remove all work from the job-list that belongs to
  ;; this user, set up the global variables current-action-time and
  ;; configuration-user appropriately, and then process the new configuration
  ;; file for the user.

  (do () ((and (if (release-arbiter interrupt-required)
                   (begin (kill (getpid) SIGHUP) #f)
                   #t)
               (null? hup-received-for)))
    (try-arbiter pending-lock)
    (let ((user (car hup-received-for)))
      (set! hup-received-for (cdr hup-received-for))
      (release-arbiter pending-lock)
      (set! configuration-user (getpw user))
      (let ((uid (passwd:uid configuration-user))
            (old-job-list job-list))
        (set! current-action-time (current-time))
        (set! job-list
              (append
               (list-head old-job-list system-jobs)
               (begin (set! job-list '())
                      (read-vixie-file (string-append "/var/cron/tabs/" user))
                      job-list)
               (remove (lambda (job) (eqv? (passwd:uid (job:user job)) uid))
                       (list-tail old-job-list system-jobs)))))))
  
  
  ;; Compute the amount of time that we must sleep until the next job is due to
  ;; run.
  
  (let* ((next-jobs      (find-next-jobs))
         (next-time      (car next-jobs))
         (next-jobs-list (cdr next-jobs))
         (sleep-time     (if next-time (- next-time (current-time))
                                       #f)))

    
    ;; If an update signal has just come in, or there are no current jobs and a
    ;; pause operation has been interrupted (presumably by a SIGHUP), or the
    ;; sleep operation has been interrupted (presumably by a SIGHUP), then undo
    ;; the latest time calculations and jump back to the top of the loop where
    ;; the pending updates will be dealt with.
    ;;
    ;; Otherwise, when we wake from our sleep, first try to collect as many
    ;; child zombies as possible from previous job runs, then run the current
    ;; set of jobs (on the next-jobs-list).
    
    (if (and (null? hup-received-for)
                                  ;; ! If a signal occurs now, we won't see it
                                  ;; until the next signal.
             (eqv? 0 (cond ((not sleep-time) (pause) 1)
                           ((> sleep-time 0) (sleep sleep-time))
                           (else 0))))
        (run-jobs next-jobs-list)))
  
    (do () ((or (<= number-children 0)
                (eqv? (car (waitpid WAIT_ANY WNOHANG)) 0)))
      (set! number-children (- number-children 1)))

    (main-loop))
