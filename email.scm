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


;; This file provides the (with-mail-out action . user) procedure. This
;; procedure runs the action in a child process, allowing the user control over
;; the input and output (including standard error). The input is governed (only
;; in the case of a string action) by the placing of percentage signs in the
;; string; the first delimits the true action from the standard input, and
;; subsequent ones denote newlines to be placed into the input. The output (if
;; there actually is any) is controlled by the MAILTO environment variable. If
;; this is not defined, output is e-mailed to the user passed as argument, if
;; any, or else the owner of the action; if defined but empty then any output is
;; sunk to /dev/null; otherwise output is e-mailed to the address held in the
;; MAILTO variable.


;; An action string consists of a sequence of characters forming a command
;; executable by the shell, possibly followed by an non-escaped percentage
;; sign. The text after the percentage sign is to be fed to the command's
;; standard input, with further unescaped percents being substituted with
;; newlines. The escape character can itself be escaped.
;;
;; This regexp separates the two halves of the string, and indeed determines if
;; the second part is present.

(define action-string-regexp (make-regexp "((\\\\%|[^%])*)%(.*)$"))



;; This regexp identifies an escaped percentage sign.

(define e-percent (make-regexp "\\\\%"))


;; Function to execute some action (this may be a shell command, lamdba function
;; or list of scheme procedures) in a forked process, with the input coming from
;; the string, and output (including the error output) being sent to a pipe
;; opened on a mail transport.

(use-modules (ice-9 popen))

(define (with-mail-out action . user)

  ;; Determine the name of the user who is to recieve the mail, looking for a
  ;; name in the optional user argument, then in the MAILTO environment
  ;; variable, and finally in the LOGNAME environment variable. (The case
  ;; MAILTO="" is dealt with specially below.)

  (let* ((mailto (getenv "MAILTO"))
         (user (cond (mailto mailto)
                     ((not (null? user)) (car user))
                     (else (getenv "LOGNAME"))))
         (parent->child (pipe))
         (child->parent (pipe))
         (child-pid (primitive-fork)))

    
    ;; The child process. Close redundant ends of pipes, remap the standard
    ;; streams, and run the action, taking care to chop off the input part of an
    ;; action string.
    
    (if (eqv? child-pid 0)
        (begin
          (close (cdr parent->child))
          (close (car child->parent))

          (dup2 (port->fdes (car parent->child)) 0)
          (close (car parent->child))
          (dup2 (port->fdes (cdr child->parent)) 1)
          (close (cdr child->parent))
          (dup2 1 2)

          (cond ((string? action)
                 (let ((match (regexp-exec action-string-regexp action)))
                   (system (if match
                               (let ((action (match:substring match 1)))
                                 (do ((match (regexp-exec e-percent action)
                                             (regexp-exec e-percent action)))
                                     ((not match))
                                   (set! action (string-append
                                                         (match:prefix match)
                                                         "%"
                                                         (match:suffix match))))
                                 action)
                               action))))
              
                ((procedure? action) (action))
                ((list? action) (primitive-eval action)))

          (primitive-exit 0)))


    ;; The parent process. Get rid of redundant pipe ends.

    (close (car parent->child))
    (close (cdr child->parent))


    ;; Put stuff to child from after '%' in command line, replacing
    ;; other %'s with newlines. Ugly or what?

    (if (string? action)
        (let ((port (cdr parent->child))
              (match (regexp-exec action-string-regexp action)))
          (if (and match
                   (match:substring match 3))
              (with-input-from-string (match:substring match 3)
                (lambda ()
                  (let loop ()
                    (let ((next-char (read-char)))
                      (if (not (eof-object? next-char))
                          (cond
                            ((char=? next-char #\%)
                             (newline port)
                             (loop))
                            ((char=? next-char #\\)
                             (let ((escape (read-char)))
                               (if (eof-object? escape)
                                   (display #\\ port)
                                   (if (char=? escape #\%)
                                       (begin
                                         (display #\% port)
                                         (loop))
                                       (begin
                                         (display #\\ port)
                                         (display escape port)
                                         (loop))))))
                            (else
                             (display next-char port)
                             (loop)))))))))))

    
    ;; So the child process doesn't hang on to its input expecting more stuff.
    
    (close (cdr parent->child))


    ;; That's got streaming into the child's input out of the way, now we stream
    ;; the child's output to a mail sink, but only if there is something there
    ;; in the first place.
    
    (if (eof-object? (peek-char (car child->parent)))

        (read-char (car child->parent))
        
        (begin
          (set-current-output-port (if (and (string? mailto)
                                            (string=? mailto ""))
                                       (open-output-file "/dev/null")
                                       (open-output-pipe
                                          (string-append config-sendmail
                                                         " "
                                                         user))))
          (set-current-input-port (car child->parent))
          (display "To: ") (display user) (newline)
          (display "From: mcron") (newline)
          (display (string-append "Subject: " user "@" (gethostname)))
          (newline)
          (newline)

          (do ((next-char (read-char) (read-char)))
              ((eof-object? next-char))
            (display next-char))))

    (close (car child->parent))

    (waitpid child-pid)))
