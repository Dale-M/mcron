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


;; Apart from the collecting of options and the handling of --help and --version
;; (which are done in the main.scm file), this file provides all the
;; functionality of the crontab personality. It is designed to be loaded and run
;; once, and then the calling program can exit and the crontab program will have
;; completed its function.



;; Procedure to communicate with running cron daemon that a user has modified
;; his crontab. The user name is written to the /var/cron/socket UNIX socket.

(define (hit-server user-name)
  (catch #t (lambda ()
              (let ((socket (socket AF_UNIX SOCK_STREAM 0)))
                (connect socket AF_UNIX config-socket-file)
                (display user-name socket)
                (close socket)))
         (lambda (key . args)
           (display "Warning: a cron daemon is not running.\n"))))



;; Procedure to scan a file containing one user name per line (such as
;; /var/cron/allow and /var/cron/deny), and determine if the given name is in
;; there. The procedure returns #t, #f, or '() if the file does not exist.

(define (in-access-file? file name)
  (catch #t (lambda ()
              (with-input-from-file file (lambda ()
                (let loop ((input (read-line)))
                  (if (eof-object? input)
                      #f
                      (if (string=? input name)
                          #t
                          (loop (read-line))))))))
         (lambda (key . args) '())))



;; This program should have been installed SUID root. Here we get the passwd
;; entry for the real user who is running this program.

(define crontab-real-user (passwd:name (getpw (getuid))))



;; If the real user is not allowed to use crontab due to the /var/cron/allow
;; and/or /var/cron/deny files, bomb out now.

(if (or (eq? (in-access-file? config-allow-file crontab-real-user) #f)
        (eq? (in-access-file? config-deny-file crontab-real-user) #t))
    (begin
      (display "Access denied by system operator.\n")
      (primitive-exit 6)))



;; Check that no more than one of the mutually exclusive options are being used.

(if (> (+ (if (option-ref options 'edit #f) 1 0)
          (if (option-ref options 'list #f) 1 0)
          (if (option-ref options 'remove #f) 1 0))
       1)
    (begin
      (display "crontab: Only one of options -e, -l or -r can be used.\n")
      (primitive-exit 7)))



;; Check that a non-root user is trying to read someone else's files.

(if (and (not (eqv? (getuid) 0))
         (option-ref options 'user #f))
    (begin (display "crontab: Only root can use the -u option.\n")
           (primitive-exit 8)))



;; Iff the --user option is given, the crontab-user may be different from the
;; real user.

(define crontab-user (option-ref options 'user crontab-real-user))



;; So now we know which crontab file we will be manipulating.

(define crontab-file (string-append config-spool-dir "/" crontab-user))



;; There are four possible sub-personalities to the crontab personality: list,
;; remove, edit and replace (when the user uses no options but supplies file
;; names on the command line).

(cond


 ;; In the list personality, we simply open the crontab and copy it
 ;; character-by-character to the standard output. If anything goes wrong, it
 ;; can only mean that this user does not have a crontab file.
 
 ((option-ref options 'list #f)
  (catch #t (lambda ()
              (with-input-from-file crontab-file (lambda ()
                 (do ((input (read-char) (read-char)))
                     ((eof-object? input))
                   (display input)))))
         (lambda (key . args)
           (display (string-append "No crontab for "
                                   crontab-user
                                   " exists.\n")))))


 ;; In the edit personality, we determine the name of a temporary file and an
 ;; editor command, copy an existing crontab file (if it is there) to the
 ;; temporary file, making sure the ownership is set so the real user can edit
 ;; it; once the editor returns we try to read the file to check that it is
 ;; parseable (but do nothing more with the configuration), and if it is okay
 ;; (this program is still running!) we move the temporary file to the real
 ;; crontab, wake the cron daemon up, and remove the temporary file.

 ((option-ref options 'edit #f)
  (let ((temp-file (string-append config-tmp-dir
                                  "/crontab."
                                  (number->string (getpid)))))
    (catch #t (lambda () (copy-file crontab-file temp-file))
              (lambda (key . args) (with-output-to-file temp-file noop)))
    (chown temp-file (getuid) (getgid))
    (system (string-append (or (getenv "VISUAL") (getenv "EDITOR") "vi")
                           " "
                           temp-file))
    (read-vixie-file temp-file)
    (copy-file temp-file crontab-file)
    (delete-file temp-file)
    (hit-server crontab-user)))


 ;; In the remove personality we simply make an effort to delete the crontab and
 ;; wake the daemon. No worries if this fails.

 ((option-ref options 'remove #f)
  (catch #t (lambda () (delete-file crontab-file)
                       (hit-server crontab-user))
            noop))


 ;; In the case of the replace personality we loop over all the arguments on the
 ;; command line, and for each one parse the file to make sure it is parseable
 ;; (but subsequently ignore the configuration), and all being well we copy it
 ;; to the crontab location; we deal with the standard input in the same way but
 ;; different. :-)  In either case the server is woken so that it will read the
 ;; newly installed crontab.

 ((not (null? (option-ref options '() '())))
  (let ((input-file (car (option-ref options '() '()))))
    (if (string=? input-file "-")
        (let ((input-string (stdin->string)))
          (read-vixie-port (open-input-string input-string))
          (with-output-to-file crontab-file (lambda ()
                                              (display input-string))))
        (begin
          (read-vixie-file input-file)
          (copy-file input-file crontab-file))))
  (hit-server crontab-user))
 
 
 ;; The user is being silly. The message here is identical to the one Vixie cron
 ;; used to put out, for total compatibility.

 (else
  (display "crontab: usage error: file name must be specified for replace.\n")
  (primitive-exit 15)))
