;;;; environment.scm -- interact with the job process environment
;;; Copyright © 2003 Dale Mellor <dale_mellor@users.sourceforge.net>
;;; Copyright © 2015, 2016, 2018 Mathieu Lirzin <mthl@gnu.org>
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

;;;; Commentary:
;;;
;;; Define the variable current-environment-mods, and the procedures
;;; append-environment-mods (which is available to user configuration files),
;;; clear-environment-mods and modify-environment.  The idea is that the
;;; current-environment-mods is a list of pairs of environment names and
;;; values, and represents the cumulated environment settings in a
;;; configuration file.  When a job definition is seen in a configuration file,
;;; the current-environment-mods are copied into the internal job description,
;;; and when the job actually runs these environment modifications are applied
;;; to the UNIX environment in which the job runs.
;;;
;;;; Code:

(define-module (mcron environment)
  #:use-module (srfi srfi-111)
  #:export (modify-environment
            clear-environment-mods
            append-environment-mods
            get-current-environment-mods-copy))

;;;
;;; Configuration files
;;;

(define %current-environment-mods
  ;; Global variable containing an alist of environment variables populated as
  ;; we parse configuration files.
  (box '()))

(define* (get-current-environment-mods-copy
          #:key (environ %current-environment-mods))
  "Return a snapshot of the current environment modifications from ENVIRON.
This snapshot is a copy of the environment so that modifying it doesn't
impact ENVIRON."
  ;; Each time a job is registered we should call this procedure.
  (list-copy (unbox environ)))

(define* (clear-environment-mods #:key (environ %current-environment-mods))
  "Remove all entries in the ENVIRON environment."
  ;; When we start to parse a new configuration file, we want to start with a
  ;; fresh environment (actually an umodified version of the pervading mcron
  ;; environment) by calling this procedure.
  (set-box! environ '()))

(define* (append-environment-mods name value
                                  #:key (environ %current-environment-mods))
  "Set NAME to VALUE in the ENVIRON environment.  If VALUES is #f then NAME is
considered unset."
  ;; This procedure is used implicitly by the Vixie parser, and can be used
  ;; directly by users in scheme configuration files.
  (set-box! environ (append (unbox environ) `((,name . ,value))))
  ;; XXX: The return value is purely for the convenience of the
  ;; '(@ (mcron vixie-specification) parse-vixie-environment)'.
  #t)

;;;
;;; Job runtime
;;;

(define (modify-environment env passwd-entry)
  "Modify the environment (in the UNIX sense) by setting the variables from
ENV and some default ones which are modulated by PASSWD-ENTRY.  \"LOGNAME\"
and \"USER\" environment variables can't be overided by ENV.  ENV must be an
alist which associate environment variables to their value.  PASSWD-ENTRY must
be an object representing user information which corresponds to a valid entry
in /etc/passwd.  The return value is not specified."
  (for-each (lambda (pair) (setenv (car pair) (cdr pair)))
            (let ((home-dir  (passwd:dir passwd-entry))
                  (user-name (passwd:name passwd-entry)))
              (append
               ;; Default environment variables which can be overided by ENV.
               `(("HOME"    . ,home-dir)
                 ("CWD"     . ,home-dir)
                 ("SHELL"   . ,(passwd:shell passwd-entry))
                 ("TERM"    . #f)
                 ("TERMCAP" . #f))
               env
               ;; Environment variables with imposed values.
               `(("LOGNAME" . ,user-name)
                 ("USER"    . ,user-name))))))
