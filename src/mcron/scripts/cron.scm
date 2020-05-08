;;;; cron -- daemon for running jobs at scheduled times
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


(define-module (mcron scripts cron)
  #:use-module (ice-9 ftw)
  #:use-module (mcron base)
  #:use-module (mcron config)
  #:use-module (mcron job-specifier)
  #:use-module (mcron utils)
  #:use-module (mcron vixie-specification)
  #:use-module (srfi srfi-2)
  #:export (main))



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
      (for-each
       (λ (user)
         (and-let* ((entry (user-entry user))) ;crontab without user?
           (set-configuration-user entry)
           (catch-mcron-error
            (read-vixie-file (string-append config-spool-dir "/" user)))))
       (scandir config-spool-dir)))
    (λ (key . args)
      (mcron-error 4
        "You do not have permission to access the system crontabs."))))

(define (%process-files noetc?)
  ;; Clear MAILTO so that outputs are sent to the various users.
  (setenv "MAILTO" #f)
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

(define (main --schedule --noetc)
    (when  config-debug  (debug-enable 'backtrace))

    (cond  ((not (zero? (getuid)))
               (mcron-error 16
                   "This program must be run by the root user (and should"
                   " have been installed as such)."))
           ((access? config-pid-file F_OK)
               (mcron-error 1
                   "A cron daemon is already running.\n  (If you are sure"
                   " this is not true, remove the file\n   "
                   config-pid-file ".)"))
           (else
               (cond (--schedule
                      => (λ (count)
                           (display-schedule (max 1 (string->number count)))
                           (exit 0))))
               (%process-files --noetc)))

  ;; Daemonize ourself.
  (unless  (eq? 0 (primitive-fork))  (exit 0))
  (setsid)

  ;; Set up process signal handlers, as signals are the only way to terminate
  ;; the daemon and we MUST be graceful in defeat.
  (for-each   (λ (x)  (sigaction  x
                          (λ (sig)  (catch #t
                                           (λ ()
                                             (delete-file config-pid-file)
                                             (delete-file config-socket-file))
                                           noop)
                             (exit EXIT_FAILURE))))
                '(SIGTERM SIGINT SIGQUIT SIGHUP))

  ;; We can now write the PID file.
  (with-output-to-file  config-pid-file
                        (λ () (display (getpid)) (newline)))

  (parameterize ((%log-format  (option-ref opts 'log-format (%log-format)))
                 (%date-format (option-ref opts 'date-format (%date-format))))
    ;; Forever execute the 'run-job-loop', and when it drops out (can
    ;; only be because a message has come in on the socket) we
    ;; process the socket request before restarting the loop again.
    (let ((fdes-list (cron-file-descriptors)))
      (while #t
        (run-job-loop fdes-list)
        (unless (null? fdes-list) (process-update-request fdes-list))))))
