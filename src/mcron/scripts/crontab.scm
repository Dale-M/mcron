;;;; crontab -- edit user's cron tabs
;;; Copyright © 2003, 2004 Dale Mellor <dale_mellor@users.sourceforge.net>
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

(define-module (mcron scripts crontab)
  #:use-module (ice-9 rdelim)
  #:use-module (mcron config)
  #:use-module (mcron utils)
  #:use-module (mcron vixie-specification)
  #:export (main))

(define* (show-help)
  (display "Usage: crontab [-u user] file
       crontab [-u user] { -e | -l | -r }
               (default operation is replace, per 1003.2)
       -e      (edit user's crontab)
       -l      (list user's crontab)
       -r      (delete user's crontab")
  (newline)
  (show-package-information))

(define %options
  '((user     (single-char #\u) (value #t))
    (edit     (single-char #\e) (value #f))
    (list     (single-char #\l) (value #f))
    (remove   (single-char #\r) (value #f))
    (version  (single-char #\v) (value #f))
    (help     (single-char #\h) (value #f))))


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
           (show-version "crontab")
           (exit 0)))
    (let ((hit-server
           (λ (user-name)
             ;; Procedure to communicate with running cron daemon that a user
             ;; has modified his crontab. The user name is written to the
             ;; /var/cron/socket UNIX socket.
             (catch #t
               (λ ()
                 (let ((socket (socket AF_UNIX SOCK_STREAM 0)))
                   (connect socket AF_UNIX config-socket-file)
                   (display user-name socket)
                   (close socket)))
               (λ (key . args)
                 (display "Warning: a cron daemon is not running.\n")))))

          ;; Procedure to scan a file containing one user name per line (such
          ;; as /var/cron/allow and /var/cron/deny), and determine if the
          ;; given name is in there. The procedure returns #t, #f, or '() if
          ;; the file does not exist.
          (in-access-file?
           (λ (file name)
             (catch #t
               (λ ()
                 (with-input-from-file file
                   (λ ()
                     (let loop ((input (read-line)))
                       (if (eof-object? input)
                           #f
                           (if (string=? input name)
                               #t
                               (loop (read-line))))))))
               (λ (key . args) '()))))

          ;; This program should have been installed SUID root. Here we get
          ;; the passwd entry for the real user who is running this program.
          (crontab-real-user (passwd:name (getpw (getuid)))))

      ;; If the real user is not allowed to use crontab due to the
      ;; /var/cron/allow and/or /var/cron/deny files, bomb out now.
      (if (or (eq? (in-access-file? config-allow-file crontab-real-user) #f)
              (eq? (in-access-file? config-deny-file crontab-real-user) #t))
          (mcron-error 6 "Access denied by system operator."))

      ;; Check that no more than one of the mutually exclusive options are
      ;; being used.
      (when (> (+ (if (option-ref opts 'edit #f) 1 0)
                  (if (option-ref opts 'list #f) 1 0)
                  (if (option-ref opts 'remove #f) 1 0))
               1)
        (mcron-error 7 "Only one of options -e, -l or -r can be used."))

      ;; Check that a non-root user is trying to read someone else's files.
      (when (and (not (zero? (getuid)))
                 (option-ref opts 'user #f))
        (mcron-error 8 "Only root can use the -u option."))

      (letrec* (;; Iff the --user option is given, the crontab-user may be
                ;; different from the real user.
                (crontab-user (option-ref opts 'user crontab-real-user))
                ;; So now we know which crontab file we will be manipulating.
                (crontab-file (string-append config-spool-dir "/" crontab-user))
                ;; Display the prompt and wait for user to type his
                ;; choice. Return #t if the answer begins with 'y' or 'Y',
                ;; return #f if it begins with 'n' or 'N', otherwise ask
                ;; again.
                (get-yes-no (λ (prompt . re-prompt)
                              (if (not (null? re-prompt))
                                  (display "Please answer y or n.\n"))
                              (display (string-append prompt " "))
                              (let ((r (read-line)))
                                (if (not (string-null? r))
                                    (case (string-ref r 0)
                                      ((#\y #\Y) #t)
                                      ((#\n #\N) #f)
                                      (else (get-yes-no prompt #t)))
                                    (get-yes-no prompt #t))))))
        ;; There are four possible sub-personalities to the crontab
        ;; personality: list, remove, edit and replace (when the user uses no
        ;; options but supplies file names on the command line).
        (cond
         ;; In the list personality, we simply open the crontab and copy it
         ;; character-by-character to the standard output. If anything goes
         ;; wrong, it can only mean that this user does not have a crontab
         ;; file.
         ((option-ref opts 'list #f)
          (catch #t
            (λ ()
              (with-input-from-file crontab-file
                (λ ()
                  (do ((input (read-char) (read-char)))
                      ((eof-object? input))
                    (display input)))))
            (λ (key . args)
              (display (string-append "No crontab for "
                                      crontab-user
                                      " exists.\n")))))

         ;; In the edit personality, we determine the name of a temporary file
         ;; and an editor command, copy an existing crontab file (if it is
         ;; there) to the temporary file, making sure the ownership is set so
         ;; the real user can edit it; once the editor returns we try to read
         ;; the file to check that it is parseable (but do nothing more with
         ;; the configuration), and if it is okay (this program is still
         ;; running!) we move the temporary file to the real crontab, wake the
         ;; cron daemon up, and remove the temporary file. If the parse fails,
         ;; we give user a choice of editing the file again or quitting the
         ;; program and losing all changes made.
         ((option-ref opts 'edit #f)
          (let ((temp-file (string-append config-tmp-dir
                                          "/crontab."
                                          (number->string (getpid)))))
            (catch #t
              (λ () (copy-file crontab-file temp-file))
              (λ (key . args) (with-output-to-file temp-file noop)))
            (chown temp-file (getuid) (getgid))
            (let retry ()
              (system (string-append
                       (or (getenv "VISUAL") (getenv "EDITOR") "vi")
                       " "
                       temp-file))
              (catch 'mcron-error
                (λ () (read-vixie-file temp-file))
                (λ (key exit-code . msg)
                  (apply mcron-error 0 msg)
                  (if (get-yes-no "Edit again?")
                      (retry)
                      (begin
                        (mcron-error 0 "Crontab not changed")
                        (primitive-exit 0))))))
            (copy-file temp-file crontab-file)
            (delete-file temp-file)
            (hit-server crontab-user)))

         ;; In the remove personality we simply make an effort to delete the
         ;; crontab and wake the daemon. No worries if this fails.
         ((option-ref opts 'remove #f)
          (catch #t
            (λ ()
              (delete-file crontab-file)
              (hit-server crontab-user))
            noop))

         ;; XXX: This comment is wrong.
         ;; In the case of the replace personality we loop over all the
         ;; arguments on the command line, and for each one parse the file to
         ;; make sure it is parseable (but subsequently ignore the
         ;; configuration), and all being well we copy it to the crontab
         ;; location; we deal with the standard input in the same way but
         ;; different. :-) In either case the server is woken so that it will
         ;; read the newly installed crontab.
         ((not (null? (option-ref opts '() '())))
          (let ((input-file (car (option-ref opts '() '()))))
            (catch-mcron-error
             (if (string=? input-file "-")
                 (let ((input-string (read-string)))
                   (read-vixie-port (open-input-string input-string))
                   (with-output-to-file crontab-file
                     (λ () (display input-string))))
                 (begin
                   (read-vixie-file input-file)
                   (copy-file input-file crontab-file))))
            (hit-server crontab-user)))

         ;; The user is being silly. The message here is identical to the one
         ;; Vixie cron used to put out, for total compatibility.
         (else (mcron-error 15
                 "usage error: file name must be specified for replace.")))))))
