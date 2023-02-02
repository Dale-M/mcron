;;;; crontab -- edit user's cron tabs
;;; Copyright © 2003, 2004 Dale Mellor <>
;;; Copyright © 2016 Mathieu Lirzin <mthl@gnu.org>
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
(define-module (mcron scripts crontab-access)
  #:use-module (ice-9 rdelim)
  #:use-module (mcron config)
  #:use-module (mcron utils)
  #:use-module (mcron vixie-specification)
  #:export (main))

(define (hit-server user-name)
  "Tell the running cron daemon that the user corresponding to
USER-NAME has modified his crontab.  USER-NAME is written to the
'/var/cron/socket' UNIX socket."
  (catch #t
    (lambda ()
      (let ((socket (socket AF_UNIX SOCK_STREAM 0)))
        (connect socket AF_UNIX config-socket-file)
        (display user-name socket)
        (close socket)))
    (lambda (key . args)
      (display "Warning: a cron daemon is not running.\n"))))

(define (in-access-file? file name)
  "Scan FILE which should contain one user name per line (such as
'/var/cron/allow' and '/var/cron/deny').  Return #t if NAME is in there, and
#f otherwise.  If FILE cannot be opened, a value that is neither #t nor #f
is returned."
  (catch #t
    (lambda ()
      (with-input-from-file file
        (lambda ()
          (let loop ((input (read-line)))
            (cond ((eof-object? input)
                   #f)
                  ((string=? input name)
                   #t)
                  (else
                   (loop (read-line))))))))
    (const '())))

(define (main --user --replace --list --remove)
  (when config-debug  (debug-enable 'backtrace))
  (let ((crontab-real-user
         ;; This program should have been installed SUID root. Here we get
         ;; the passwd entry for the real user who is running this program.
         (passwd:name (getpw (getuid)))))

    ;; If the real user is not allowed to use crontab due to the
    ;; /var/cron/allow and/or /var/cron/deny files, bomb out now.
    (if (or (eq? (in-access-file? config-allow-file crontab-real-user) #f)
            (eq? (in-access-file? config-deny-file crontab-real-user) #t))
        (mcron-error 6 "Access denied by system operator."))

    ;; Check that no more than one of the mutually exclusive options are
    ;; being used.
    (when (<  1  (+ (if --list 1 0) (if --remove 1 0) (if --replace 1 0)))
      (mcron-error 7 "Only one of options -l, -r or -R can be used."))

    ;; Check that a non-root user is trying to read someone else's files.
    (when (and (not (zero? (getuid))) --user)
      (mcron-error 8 "Only root can use the -u option."))

    ;; Crontabs being written should not have global or group access.
    (umask #o077)

    (letrec* ( ;; Iff the --user option is given, the crontab-user may be
              ;; different from the real user.
              (crontab-user (or --user crontab-real-user))
              ;; So now we know which crontab file we will be manipulating.
              (crontab-file
               (string-append config-spool-dir "/" crontab-user)))
      ;; There are three possible actions: list, remove, and replace (via
      ;; stdin).
      (cond
       ;; In the remove personality we simply make an effort to delete the
       ;; crontab and wake the daemon. No worries if this fails.
       (--remove (catch #t (λ ()  (delete-file crontab-file)
                              (hit-server crontab-user))
                   noop))

       ;; Read crontab from stdin, verify it, replace it, wake daemon.
       (--replace
        (let ((input-string (read-string)))
          (catch-mcron-error
           (read-vixie-port (open-input-string input-string))
           (unless (file-exists? config-spool-dir)
             (mkdir config-spool-dir #o700))
           (with-output-to-file crontab-file
             (λ () (display input-string))))
          (hit-server crontab-user)))

       ;; In the list personality, we simply open the crontab and copy it
       ;; character-by-character to the standard output. If anything goes
       ;; wrong, it can only mean that this user does not have a crontab
       ;; file.
       (else ;; --list or no action specified
        (catch #t
          (λ ()
            (with-input-from-file crontab-file
              (λ ()
                (do ((input (read-char) (read-char)))
                    ((eof-object? input))
                  (display input)))))
          (λ (key . args)
            (mcron-error 17 "No crontab for " crontab-user " exists.\n"))))))))
