;;;; cron -- daemon for running jobs at scheduled times
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

(define-module (mcron scripts cron)
  #:use-module (mcron base)
  #:use-module (mcron config)
  #:use-module (mcron job-specifier)
  #:use-module (mcron main)
  #:use-module (mcron vixie-specification)
  #:use-module (srfi srfi-2)
  #:export (main))

(define (show-help)
  (display "Usage: cron [OPTIONS]
Unless an option is specified, run a cron daemon as a detached process,
reading all the information in the users' crontabs and in /etc/crontab.

  -v, --version             Display version
  -h, --help                Display this help message
  -sN, --schedule[=]N       Display the next N jobs that will be run by cron
  -n, --noetc               Do not check /etc/crontab for updates (HIGHLY
                              RECOMMENDED).")
  (newline)
  (show-package-information))

(define %options
  `((schedule (single-char #\s) (value #t)
              (predicate ,(λ (str) (string->number str))))
    (noetc    (single-char #\n) (value #f))
    (version  (single-char #\v) (value #f))
    (help     (single-char #\h) (value #f))))

(define (delete-run-file)
  "Remove the /var/run/cron.pid file so that crontab and other invocations of
cron don't get the wrong idea that a daemon is currently running.  This
procedure is called from the C front-end whenever a terminal signal is
received."
  (catch #t
    (λ ()
      (delete-file config-pid-file)
      (delete-file config-socket-file))
    noop)
  (quit))

(define (cron-file-descriptors)
  "Establish a socket to listen for updates from a crontab program, and return
a list containing the file descriptors correponding to the files read by
crontab.  This requires that command-type is 'cron."
  (catch #t
    (λ ()
      (let ((sock (socket AF_UNIX SOCK_STREAM 0)))
        (bind sock AF_UNIX config-socket-file)
        (listen sock 5)
        (list sock)))
    (λ (key . args)
      (delete-file config-pid-file)
      (mcron-error 1 "Cannot bind to UNIX socket " config-socket-file))))

(define (process-files-in-system-directory)
  "Process all the files in the crontab directory.  When the job procedure is
run on behalf of the configuration files, the jobs are registered on the
system with the appropriate user.  Only root should be able to perform this
operation.  The permissions on the /var/cron/tabs directory enforce this."

  (define (user-entry name)
    ;; Return the user database entry if NAME is valid, otherwise #f.
    (false-if-exception (getpwnam name)))

  (catch #t
    (λ ()
      (for-each-file
       (λ (user)
         (and-let* ((entry (user-entry user))) ;crontab without user?
           (set-configuration-user entry)
           (catch-mcron-error
            (read-vixie-file (string-append config-spool-dir "/" user)))))
       config-spool-dir))
    (λ (key . args)
      (mcron-error 4
        "You do not have permission to access the system crontabs."))))

(define (%process-files schedule? noetc?)
  ;; XXX: What is this supposed to do?
  (when schedule?
    (with-output-to-file config-pid-file noop))
  ;; Clear MAILTO so that outputs are sent to the various users.
  (setenv "MAILTO" #f)
  ;; XXX: At compile time, this yields a "possibly unbound variable" warning,
  ;; but this is OK since it is bound in the C wrapper.
  (c-set-cron-signals)
  ;; Having defined all the necessary procedures for scanning various sets of
  ;; files, we perform the actual configuration of the program depending on
  ;; the personality we are running as. If it is mcron, we either scan the
  ;; files passed on the command line, or else all the ones in the user's
  ;; .config/cron (or .cron) directory. If we are running under the cron
  ;; personality, we read the /var/cron/tabs directory and also the
  ;; /etc/crontab file.
  (process-files-in-system-directory)
  (use-system-job-list)
  (catch-mcron-error
   (read-vixie-file "/etc/crontab" parse-system-vixie-line))
  (use-user-job-list)
  (unless noetc?
    (display "\
WARNING: cron will check for updates to /etc/crontab EVERY MINUTE. If you do
not use this file, or you are prepared to manually restart cron whenever you
make a change, then it is HIGHLY RECOMMENDED that you use the --noetc
option.\n")
    (set-configuration-user "root")
    (job '(- (next-minute-from (next-minute)) 6)
         check-system-crontab
         "/etc/crontab update checker.")))


;;;
;;; Entry point.
;;;

(define* (main #:optional (args (command-line)))
  (let ((opts (parse-args args %options)))
    (when config-debug
      (debug-enable 'backtrace))
    (cond
     ((option-ref opts 'help #f)
      (show-help)
      (exit 0))
     ((option-ref opts 'version #f)
      (show-version "cron")
      (exit 0))
     ((not (zero? (getuid)))
      (mcron-error 16
        "This program must be run by the root user (and should"
        " have been installed as such)."))
     ((access? config-pid-file F_OK)
      (mcron-error 1
        "A cron daemon is already running.\n  (If you are sure"
        " this is not true, remove the file\n   "
        config-pid-file ".)"))
     (else
      (%process-files (option-ref opts 'schedule #f)
                      (option-ref opts 'noetc #f))
      (cond ((option-ref opts 'schedule #f) ;display jobs schedule
             => (λ (count)
                  (display (get-schedule (max 1 (string->number count))))
                  (exit 0)))
            (else (case (primitive-fork) ;run the daemon
                    ((0)
                     (setsid)
                     ;; we can now write the PID file.
                     (with-output-to-file config-pid-file
                       (λ () (display (getpid)) (newline))))
                    (else (exit 0)))))
      ;; Forever execute the 'run-job-loop', and when it drops out (can
      ;; only be because a message has come in on the socket) we
      ;; process the socket request before restarting the loop again.
      (catch-mcron-error
       (let ((fdes-list (cron-file-descriptors)))
         (while #t
           (run-job-loop fdes-list)
           (unless (null? fdes-list)
             (process-update-request fdes-list)))))))))
