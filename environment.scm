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



;; This file defines the global variable current-environment-mods, and the
;; procedures append-environment-mods (which is available to user configuration
;; files), clear-environment-mods, modify-environment, and
;; parse-vixie-environment. The idea is that the current-environment-mods is a
;; list of pairs of environment names and values, and represents the cumulated
;; environment settings in a configuration file. When a job definition is seen
;; in a configuration file, the current-environment-mods are copied into the
;; internal job description, and when the job actually runs these environment
;; modifications are applied to the UNIX environment in which the job runs.



;; The env-alist is an association list of variable names and values. Variables
;; later in the list will take precedence over variables before. We return a
;; fixed-up version in which some variables are given specific default values
;; (which the user can override), and one variable which the user is not allowed
;; to control is added at the end of the list.

(define (impose-default-environment env-alist passwd-entry)
  (append (list (cons "HOME" (passwd:dir passwd-entry))
                (cons "CWD" (passwd:dir passwd-entry))
                (cons "SHELL" (passwd:shell passwd-entry))
                '("TERM" . #f)
                '("TERMCAP" . #f))
          env-alist
          (list (cons "LOGNAME" (passwd:name passwd-entry))
                (cons "USER" (passwd:name passwd-entry)))))




;; Modify the UNIX environment for the current process according to the given
;; association list of variables, with the default variable values imposed.

(define (modify-environment env-alist passwd-entry)
  (for-each (lambda (variable)
              (setenv (car variable) (cdr variable)))
            (impose-default-environment env-alist passwd-entry)))




;; As we parse configuration files, we build up an alist of environment
;; variables here.

(define current-environment-mods '())




;; When we start to parse a new configuration file, we want to start with a
;; fresh environment (actually an umodified version of the pervading mcron
;; environment).

(define (clear-environment-mods)
  (set! current-environment-mods '()))




;; Procedure to add another environment setting to the alist above. This is used
;; both implicitly by the Vixie parser, and can be used directly by users in
;; scheme configuration files. The return value is purely for the convenience of
;; the parse-vixie-environment procedure below.

(define (append-environment-mods name value)
  (set! current-environment-mods (append current-environment-mods
                                         (list (cons name value))))
  #t)




;; Procedure to act on an environment variable specification in a Vixie-style
;; configuration file, by adding an entry to the alist above. Returns #t if the
;; operation was successful, #f if the line could not be interpreted as an
;; environment specification.

(define parse-vixie-environment-regexp1
  (make-regexp
   "^[ \t]*([[:alpha:]_][[:alnum:]_]*)[ \t]*=[ \t]*\"(.*)\"[ \t]*$"))
(define parse-vixie-environment-regexp2
  (make-regexp
   "^[ \t]*([[:alpha:]_][[:alnum:]_]*)[ \t]*=[ \t]*\'(.*)\'[ \t]*$"))
(define parse-vixie-environment-regexp3
  (make-regexp
   "^[ \t]*([[:alpha:]_][[:alnum:]_]*)[ \t]*=[ \t]*(.*[^ \t])[ \t]*$"))
(define parse-vixie-environment-regexp4
  (make-regexp
   "^[ \t]*([[:alpha:]_][[:alnum:]_]*)[ \t]*=[ \t]*$"))

(use-modules (srfi srfi-2))

(define (parse-vixie-environment string)
  (let ((match (or (regexp-exec parse-vixie-environment-regexp1 string)
                   (regexp-exec parse-vixie-environment-regexp2 string)
                   (regexp-exec parse-vixie-environment-regexp3 string))))
    (if match
        (append-environment-mods (match:substring match 1)
                                 (match:substring match 2))
        (and-let* ((match (regexp-exec parse-vixie-environment-regexp4 string)))
                  (append-environment-mods (match:substring match 1) #f)))))
