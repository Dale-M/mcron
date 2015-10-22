;;   Copyright (C) 2003, 2015 Dale Mellor
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



;; This file defines the variable current-environment-mods, and the procedures
;; append-environment-mods (which is available to user configuration files),
;; clear-environment-mods and modify-environment. The idea is that the
;; current-environment-mods is a list of pairs of environment names and values,
;; and represents the cumulated environment settings in a configuration
;; file. When a job definition is seen in a configuration file, the
;; current-environment-mods are copied into the internal job description, and
;; when the job actually runs these environment modifications are applied to
;; the UNIX environment in which the job runs.




(define-module (mcron environment)
  #:use-module (srfi srfi-26)
  #:export (modify-environment
            clear-environment-mods
            append-environment-mods
            get-current-environment-mods-copy))
            


;; As we parse configuration files, we build up an alist of environment
;; variables here.

(define current-environment-mods '())



;; Each time a job is added to the system, we take a snapshot of the current
;; set of environment modifiers.

(define (get-current-environment-mods-copy)
  (list-copy current-environment-mods))



;; When we start to parse a new configuration file, we want to start with a
;; fresh environment (actually an umodified version of the pervading mcron
;; environment).

(define (clear-environment-mods)
  (set! current-environment-mods '()))



;; Procedure to add another environment setting to the alist above. This is
;; used both implicitly by the Vixie parser, and can be used directly by users
;; in scheme configuration files. The return value is purely for the
;; convenience of the parse-vixie-environment in the vixie-specification module
;; (yuk).

(define (append-environment-mods name value)
  (set! current-environment-mods (append current-environment-mods
                                         (list (cons name value))))
  #t)



;; Modify the UNIX environment for the current process according to the given
;; association list of variables, with the default variable values imposed.

(define (modify-environment env-alist passwd-entry)
  (for-each (lambda (a) (setenv (car a) (cdr a)))
            (let ((home-dir  (passwd:dir passwd-entry))
                  (user-name (passwd:name passwd-entry)))
              (append
               ;; Default environment variables which can be overidden by ENV.
               `(("HOME"    . ,home-dir)
                 ("CWD"     . ,home-dir)
                 ("SHELL"   . ,(passwd:shell passwd-entry))
                 ("TERM"    . #f)
                 ("TERMCAP" . #f))
               env-alist
               ;; Environment variables with imposed values.
               `(("LOGNAME" . ,user-name)
                 ("USER"    . ,user-name))))))
