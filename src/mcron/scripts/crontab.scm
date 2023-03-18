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

(define-module (mcron scripts crontab)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-1)
  #:use-module (mcron config)
  #:use-module (mcron utils)
  #:use-module (mcron vixie-specification)
  #:export (main))

;; Display the prompt and wait for user to type his choice. Return #t if the
;; answer begins with 'y' or 'Y', return #f if it begins with 'n' or 'N',
;; otherwise ask again.
(define  (get-yes-no prompt . re-prompt)
  (unless (null? re-prompt)
      (display "Please answer y or n.\n"))
  (display (string-append prompt " "))
  (let ((r (read-line)))
    (if (not (string-null? r))
        (case (string-ref r 0)
              ((#\y #\Y) #t)
              ((#\n #\N) #f)
              (else (get-yes-no prompt #t)))
      (get-yes-no prompt #t))))




;;;
;;; Entry point.
;;;

(define (main --user --edit --list --remove files)
  (when config-debug  (debug-enable 'backtrace))
  ;; Check that no more than one of the mutually exclusive options are
  ;; being used.
  (when (<  1  (+ (if --edit 1 0) (if --list 1 0) (if --remove 1 0)))
    (mcron-error 7 "Only one of options -e, -l or -r can be used."))

  ;; Check that a non-root user is trying to read someone else's files.
  ;; This will be enforced in the setuid helper 'crontab-access', but good
  ;; to let the user know early.
  (when (and (not (zero? (getuid))) --user)
    (mcron-error 8 "Only root can use the -u option."))

  ;; Crontabs being edited should not be global or group-readable.
  (umask #o077)

  (let ((user-args (if --user (list "-u" --user) '())))
    (define (usable-crontab-access? filename)
      (and=> (stat filename #f)
             (λ (st)
               (or (zero? (getuid))
                   (and (not (zero? (logand #o4000 (stat:mode st))))
                        (zero? (stat:uid st))
                        (access? filename X_OK))))))

    (define crontab-access
      (find usable-crontab-access?
            (map (λ (f) (string-append f "/crontab-access"))
                 (cons config-sbin-dir
                       (or (and=> (getenv "PATH") parse-path)
                           '())))))

    (define (exec-crontab-access . args)
      (catch #t
        (λ ()
          (apply execlp crontab-access crontab-access
                 (append user-args args))
          (mcron-error 18 "Couldn't execute `crontab-access'."))
        (λ args
          (apply mcron-error 18 "Couldn't execute `crontab-access'." args))))

    (define (crontab-access-dup2 srcs dsts closes . args)
      (let ((pid (primitive-fork)))
        (cond
         ((zero? pid)
          (for-each dup2 srcs dsts)
          (for-each close-fdes closes)
          (apply exec-crontab-access args))
         (else
          (waitpid pid)))))

    (define (try-replace file)
      (call-with-input-file file
        (λ (port)
          (crontab-access-dup2 (list (fileno port)) '(0) '() "-R"))))

    (unless crontab-access
      (mcron-error 18 "Couldn't find a usable `crontab-access'."))

    ;; There are four possible sub-personalities to the crontab
    ;; personality: list, remove, edit and replace (when the user uses no
    ;; options but supplies file names on the command line).
    (cond
     (--list (exec-crontab-access "-l"))
     (--remove (exec-crontab-access "-r"))

     ;; In the edit personality, we determine the name of a temporary file and
     ;; an editor command, copy an existing crontab file (if it is there) to
     ;; the temporary file, once the editor returns we try to replace any
     ;; existing crontab file.  If this fails, we give user a choice of
     ;; editing the file again or quitting the program and losing all changes
     ;; made.
     (--edit
      (let* ((template (string-append config-tmp-dir
                                      "/crontab."
                                      (number->string (getpid))
                                      ".XXXXXX"))
             (temp-file (call-with-port (mkstemp template "w")
                          (λ (port)
                            (crontab-access-dup2 (list (fileno port)) '(1)
                                                 '(2) "-l")
                            (chmod port #o600)
                            (port-filename port)))))
        (define (exit/cleanup status)
          (false-if-exception (delete-file temp-file))
          (primitive-exit status))

        (let retry ()
          (catch #t
            (λ () (system (string-append
                           (or (getenv "VISUAL") (getenv "EDITOR") "vi")
                           " "
                           temp-file)))
            (λ _ (exit/cleanup 1)))
          (case (status:exit-val (cdr (try-replace temp-file)))
            ((9 10 11)
             (if (get-yes-no "Edit again?")
                 (retry)
                 (begin
                   (mcron-error 0 "Crontab not changed")
                   (exit/cleanup 0))))
            (else => exit/cleanup)))))

     ;; Replace crontab with given file or stdin.  If it is a file, it must
     ;; be opened here and not in the setuid helper, to prevent accessing
     ;; unauthorized files.
     ((not (null? files))
      (let ((input-file (car files)))
        (unless (string=? input-file "-")
          (dup2 (fileno (open-file input-file "r")) 0))
        (exec-crontab-access "-R")))

     ;; The user is being silly. The message here is identical to the one
     ;; Vixie cron used to put out, for total compatibility.
     (else (mcron-error 15
             "usage error: file name must be specified for replace.")))))
