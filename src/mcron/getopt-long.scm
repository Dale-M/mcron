;;;; getopt-long.scm --- long options processing           -*- scheme -*-
;;;;
;;;; Copyright (C) 1998, 2001, 2006, 2009, 2011, 2020
;;;;                                            Free Software Foundation, Inc.
;;;;
;;;; This library is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Lesser General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 3 of the License, or (at your option) any later version.
;;;; 
;;;; This library is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Lesser General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU Lesser General Public
;;;; License along with this library; if not, write to the Free Software
;;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;;;; 02110-1301 USA

;;; Author: Russ McManus
;;;         Rewritten by Thien-Thi Nguyen
;;;         Rewritten by Dale Mellor 2020-04-14

;;; Commentary:

;;; This module implements some complex command line option parsing, in
;;; the spirit of the GNU C library function ‘getopt_long’.  Both long
;;; and short options are supported.
;;;
;;; The theory is that people should be able to constrain the set of
;;; options they want to process using a grammar, rather than some ad
;;; hoc procedure.  The grammar makes the option descriptions easy to
;;; read.
;;;
;;; ‘getopt-long’ is a procedure for parsing command-line arguments in a
;;; manner consistent with other GNU programs.  ‘option-ref’ is a procedure
;;; that facilitates processing of the ‘getopt-long’ return value.

;;; (getopt-long ARGS GRAMMAR)
;;; Parse the arguments ARGS according to the argument list grammar GRAMMAR.
;;;
;;; ARGS should be a list of strings.  Its first element should be the
;;; name of the program, and subsequent elements should be the arguments
;;; that were passed to the program on the command line.  The
;;; ‘program-arguments’ procedure returns a list of this form.
;;;
;;; GRAMMAR is a list of the form:
;;; ((OPTION (PROPERTY VALUE) ...) ...)
;;;
;;; Each OPTION should be a symbol.  ‘getopt-long’ will accept a
;;; command-line option named ‘--OPTION’.
;;; Each option can have the following (PROPERTY VALUE) pairs:
;;;
;;;   (single-char CHAR) --- Accept ‘-CHAR’ as a single-character
;;;		equivalent to ‘--OPTION’.  This is how to specify traditional
;;;		Unix-style flags.
;;;   (required? BOOL) --- If BOOL is true, the option is required.
;;;		getopt-long will raise an error if it is not found in ARGS.
;;;   (value BOOL) --- If BOOL is #t, the option accepts a value; if
;;;		it is #f, it does not; and if it is the symbol
;;;		‘optional’, the option may appear in ARGS with or
;;;		without a value.
;;;   (predicate FUNC) --- If the option accepts a value (i.e. you
;;;		specified ‘(value #t)’ or ‘(value 'optional)’ for this
;;;		option), then getopt will apply FUNC to the value, and
;;;		will not take the value if it returns #f.  FUNC should
;;;		be a procedure which accepts a string and returns a
;;;		boolean value; you may need to use quasiquotes to get it
;;;		into GRAMMAR.
;;;
;;; The (PROPERTY VALUE) pairs may occur in any order, but each
;;; property may occur only once.  By default, options do not have
;;; single-character equivalents, are not required, and do not take
;;; values.
;;;
;;; In ARGS, single-character options may be combined, in the usual
;;; Unix fashion: ("-x" "-y") is equivalent to ("-xy").  If an option
;;; accepts values, then it must be the last option in the
;;; combination; the value is the next argument.  So, for example, using
;;; the following grammar:
;;;      ((apples    (single-char #\a))
;;;       (blimps    (single-char #\b) (value #t))
;;;       (catalexis (single-char #\c) (value #t)))
;;; the following argument lists would be acceptable:
;;;    ("-a" "-b" "bang" "-c" "couth")     ("bang" and "couth" are the values
;;;                                         for "blimps" and "catalexis")
;;;    ("-ab" "bang" "-c" "couth")         (same)
;;;    ("-ac" "couth" "-b" "bang")         (same)
;;;
;;; If an option's value is optional, then ‘getopt-long’ decides whether
;;; it has a value by looking at what follows it in ARGS.  If the next
;;; element does not appear to be an option itself, and passes a
;;; predicate if given, then that element is taken to be the option's
;;; value.  Note that predicate functions are invaluable in this respect
;;; for differentiating options and option values, and in the case of
;;; options with optional values, PREDICATES REALLY SHOULD BE GIVEN.  If
;;; an option is supposed to take a numerical value, then
;;; ‘string->number’ can be used as the predicate; this will also allow
;;; negative values to be used, which would ordinarily be regarded as
;;; bad options causing the module, and the application consuming it, to
;;; bail out with an immediate exit to the operating system.
;;;
;;; The value of a long option can appear as the next element in ARGS,
;;; or it can follow the option name, separated by an ‘=’ character.
;;; Thus, using the same grammar as above, the following argument lists
;;; are equivalent:
;;;   ("--apples" "Braeburn" "--blimps" "Goodyear")
;;;   ("--apples=Braeburn" "--blimps" "Goodyear")
;;;   ("--blimps" "Goodyear" "--apples=Braeburn")
;;;
;;; If the option "--" appears in ARGS, argument parsing stops there;
;;; subsequent arguments are returned as ordinary arguments, even if
;;; they resemble options.  So, in the argument list:
;;;         ("--apples" "Granny Smith" "--" "--blimp" "Goodyear")
;;; ‘getopt-long’ will recognize the ‘apples’ option as having the value
;;; "Granny Smith", but it will not recognize the ‘blimp’ option; it
;;; will return the strings "--blimp" and "Goodyear" as ordinary
;;; argument strings.  The first "--" argument itself will *not* appear
;;; in the ordinary arguments list, although the occurrence of
;;; subsequent ones will.
;;;
;;; The ‘getopt-long’ function returns the parsed argument list as an
;;; assocation list, mapping option names --- the symbols from GRAMMAR
;;; --- onto their values, or #t if the option does not accept a value.
;;; Unused options do not appear in the alist.
;;;
;;; All arguments that are not the value of any option are returned as a
;;; list, associated with the empty list in the above returned
;;; association.
;;;
;;; ‘getopt-long’ throws an exception if:
;;; - it finds an unrecognized property in GRAMMAR
;;; - the value of the ‘single-char’ property is not a character
;;; - it finds an unrecognized option in ARGS
;;; - a required option is omitted
;;; - an option that requires an argument doesn't get one
;;; - an option that doesn't accept an argument does get one (this can
;;;   only happen using the long option ‘--opt=value’ syntax)
;;; - an option predicate fails
;;;
;;; So, for example:
;;;
;;; (define grammar
;;;   `((lockfile-dir (required? #t)
;;;                   (value #t)
;;;                   (single-char #\k)
;;;                   (predicate ,file-is-directory?))
;;;     (verbose (required? #f)
;;;              (single-char #\v)
;;;              (value #f))
;;;     (x-includes (single-char #\x))
;;;     (rnet-server (single-char #\y)
;;;                  (predicate ,string?))))
;;;
;;; (getopt-long '("my-prog" "-vk" "/tmp" "foo1" "--x-includes=/usr/include"
;;;                "--rnet-server=lamprod" "--" "-fred" "foo2" "foo3")
;;;                grammar)
;;; => ((() "foo1" "-fred" "foo2" "foo3")
;;; 	(rnet-server . "lamprod")
;;; 	(x-includes . "/usr/include")
;;; 	(lockfile-dir . "/tmp")
;;; 	(verbose . #t))

;;; (option-ref OPTIONS KEY DEFAULT)
;;; Return value in alist OPTIONS using KEY, a symbol; or DEFAULT if not
;;; found.  The return is either a string or ‘#t’, or whatever DEFAULT
;;; is.
;;;
;;; For example, using the ‘getopt-long’ return value from above:
;;;
;;; (option-ref (getopt-long ...) 'x-includes 42) => "/usr/include"
;;; (option-ref (getopt-long ...) 'not-a-key! 31) => 31

;;; Code:

(define-module (mcron getopt-long)
  #:use-module (ice-9 control)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 receive)
  #:export (getopt-long option-ref))

(define program-name (make-parameter "guile"))

(define (fatal-error fmt . args)
  (format (current-error-port) "~a: " (program-name))
  (apply format (current-error-port) fmt args)
  (newline (current-error-port))
  (exit 1))

;; name: string, required?: bool, single-char: char or #f, predicate:
;; procedure or #f, value-policy: bool or 'optional.
(define-record-type option-spec
  (%make-option-spec name required? single-char predicate value-policy)
  option-spec?
  (name         option-spec->name)
  (required?    option-spec->required?    set-option-spec-required?!)
  (single-char  option-spec->single-char  set-option-spec-single-char!)
  (predicate    option-spec->predicate    set-option-spec-predicate!)
  (value-policy option-spec->value-policy set-option-spec-value-policy!))

(define (make-option-spec name)
  (%make-option-spec name #f #f #f #f))

(define (parse-option-spec desc)
  (let ((spec (make-option-spec (symbol->string (car desc)))))
    (for-each (match-lambda
                (('required? val)
                 (set-option-spec-required?! spec val))
                (('value val)
                 (set-option-spec-value-policy! spec val))
                (('single-char val)
                 (unless (char? val)
                   (fatal-error "‘single-char’ value must be a char!"))
                 (set-option-spec-single-char! spec val))
                (('predicate pred)
                 (set-option-spec-predicate! spec pred))
                ((prop val)
                 (fatal-error "invalid getopt-long option property: " prop)))
               (cdr desc))
    spec))


;; Extract the name of a long option given that it may or may not be
;; surrounded by '--' and '=...'.
(define isolate-long-name-re (make-regexp "^(--)?([^=]+)"))

(define (isolate-long-name name)
  (and=> (regexp-exec isolate-long-name-re name)
    (λ (match) (match:substring match 2))))


;;  Whatever the presentation of the long option, make sure it is in a
;;  clean, normalized form (but this does NOT account for any value the
;;  option might have).
(define (re-present option)
  (string-append "--" (isolate-long-name option) "="))


;;  The /name/ can take the form of a long option entry on the command
;;  line, with whatever decoration that entails.  Will return #f if a
;;  spec does not exist for this named option.
(define (find-spec-long specs name)
  (and=> (isolate-long-name name)
    (λ (name) (find (λ (a) (string=? (option-spec->name a) name)) specs))))


;;  Return #f if a spec with the short /letter/ name does not exist.
(define (find-spec-short specs letter)
  (find (λ (a) (eqv? (option-spec->single-char a) letter)) specs))


;;  Take, for example, /short/='-h' to '--help='.
(define (double-up short specs)
  (and=> (find-spec-short specs (string-ref short 1))
    (λ (spec) (string-append "--" (option-spec->name spec) "="))))


;;  This procedure does whatever is necessary to put the (ostensibly)
;;  first item on the command-line into the canonical (normal) form
;;  '--item=value'; this may mean consuming the next item of the
;;  command-line (the first item of /rest/) to get the value.  Note that
;;  the value may be missing, but the '=' sign will always be there in
;;  the return.  The first item (/A/) will always be more than two
;;  characters, and the first two characters will be "--", i.e. we are
;;  processing a long option.
;;
;;  A          IN   string               The first argument on the command-line
;;  rest       IN   list of strings      The remaining items of the command-line
;;  specs      IN   list of option-spec  Options specification
;;  remnant    OUT  list of strings      The unprocessed command line
;;  processed  OUT  string               New command-line argument
(define (normalize-long-option A rest specs)
  (define (return-empty-arg) (values rest (re-present A)))
  (define (return-arg-with-value)
    (values (cdr rest) (string-append (re-present A) (car rest))))
  (cond
   ((string-index A #\=)
    ;; The argument is already in the canonical form.
    (values rest A))
   ((find-spec-long specs A)
    => (λ (spec)
          (if (cond ((null? rest) #f)
                    ((option-spec->predicate spec)
                     => (λ (pred) (pred (car rest))))
                    (else
                      (case (option-spec->value-policy spec)
                        ((#f) #f)
                        ((optional) 
                         (not (eqv? (string-ref (car rest) 0) #\-)))
                        ((#t)
                         (or (string->number (car rest))
                             (not (eqv? (string-ref (car rest) 0) #\-)))))))
              (return-arg-with-value)
              (return-empty-arg))))
   (else
    ;; We know nothing about this option, abort operations.
    (fatal-error "no such option: --~a" (isolate-long-name A)))))



;;  This procedure does whatever is necessary to put the (ostensibly)
;;  first item on the command-line into the canonical form
;;  '--item=value'; this may mean consuming the next item of the
;;  command-line (the first item of /rest/) to get the value.  Note that
;;  the value may be missing, but the '=' sign will always be there in
;;  the return.  The first item (/A/) will always be exactly two
;;  characters, and the first character will be "-", i.e. we are
;;  processing an isolated short option.
;;
;;  A          IN   string               The first argument on the command-line
;;  rest       IN   list of strings      The remaining items of the command-line
;;  specs      IN   list of option-spec  Options specification
;;  remnant    OUT  list of strings      The unprocessed command line
;;  processed  OUT  string               New command-line argument
(define (normalize-free-short-option A rest specs)
  (define (return-empty-arg) (values rest (double-up A specs)))
  (define (return-arg-with-next-value)
    (values (cdr rest) (string-append (double-up A specs) (car rest))))
  (let* ((name (string-ref A 1))
         (spec (find-spec-short specs name)))
    (if (cond ((not spec) (fatal-error "no such option: -~a" name))
              ((null? rest) #f)
              ((option-spec->predicate spec)
               => (λ (pred) (pred (car rest))))
              (else (case (option-spec->value-policy spec)
                      ((optional) (not (eq? (string-ref (car rest) 0) #\-)))
                      (else => identity))))
        (return-arg-with-next-value)
        (return-empty-arg))))



;; The /sequence/ is a string of characters from the command line, and
;; the task is to decide if those characters are a viable clumped option
;; sequence, possibly using some of the trailing characters as option
;; values, or not.
(define (viable-short sequence specs)
  (cond ((eq? 0 (string-length sequence)) #t)
        ((find-spec-short specs (string-ref sequence 0))
         => (λ (spec)
               (cond ((option-spec->predicate spec)
                      => (λ (pred) (pred (substring sequence 1))))
                     (else
                      ;; If this optionʼs /value-policy/ allows the
                      ;; option to take a value then this string is
                      ;; viable as the remainder can be taken as that
                      ;; value.  Otherwise we must assert the viability
                      ;; of the rest of the line by recursion.
                      (or (not (eq? #f (option-spec->value-policy spec)))
                          (viable-short (substring sequence 1) specs))))))
        (else #f)))



;;  This procedure does whatever is necessary to put the (ostensibly)
;;  first item on the command-line into the canonical form
;;  '--item=value'.  Note that the value may be missing, but the '='
;;  sign will always be there in the return.  The first item (/A/) will
;;  always be *more* than two characters, and the first character will
;;  be "-", i.e. we are processing a short option which is either
;;  clumped with other short options, or is clumped with its value.
;;
;;  NOTE that, contrary to the other normalize procedures, the return
;;  value of /processed/ can be #f, with the expectation that the
;;  modified /remnant/ will be re-processed.
;;
;;  A          IN   string               The first argument on the command-line
;;  rest       IN   list of strings      The remaining items of the command-line
;;  specs      IN   list of option-spec  Options specification
;;  remnant    OUT  list of strings      The unprocessed command line
;;  processed  OUT  string               New command-line argument
(define (normalize-clumped-short-option A rest specs)
  (define (declump-arg) (values (cons* (string-append "-" (substring A 1 2))
                                       (string-append "-" (substring A 2))
                                       rest)
                                #f))
  (define (construct-arg-from-clumped-value)
    (values rest (string-append (double-up A specs) (substring A 2))))
  (unless (char-alphabetic? (string-ref A 1)) (values rest A))
  (let ((spec (find-spec-short specs (string-ref A 1))))
    (if (cond ((not spec) (fatal-error "no such option: -~a" (string-ref A 1)))
              ((option-spec->predicate spec)
               => (λ (pred) (pred (substring A 2))))
              (else (case (option-spec->value-policy spec)
                      ((optional) (not (viable-short (substring A 2) specs)))
                      (else => identity))))
        (construct-arg-from-clumped-value)
        (declump-arg))))



;;  Return a version of the command-line /args/ in which all options are
;;  represented in long form with an equals sign (whether they have a
;;  value or not).
(define (normalize args specs stop-at-first-non-option?)
  (call/ec
    (λ (return)
      (let loop ((args args) (processed '()))
        (when (null? args) (return (reverse processed)))
        (define A (car args))
        (define L (string-length A))
        (define (when-loop cond normalizer)
          (when cond
            (receive (remainder-args processed-arg)
                     (normalizer A (cdr args) specs)
                     (loop
                       remainder-args
                       (if processed-arg
                           (cons processed-arg processed)
                           processed)))))
        (when (string=? "--" A)
          (return (append (reverse processed) args)))
        (when-loop (and (> L 2) (string=? (substring A 0 2) "--"))
          normalize-long-option)
        (when-loop (and (eqv? L 2) (eqv? (string-ref A 0) #\-))
          normalize-free-short-option)
        (when-loop (and (> L 1) (eqv? (string-ref A 0) #\-))
          normalize-clumped-short-option)
        (if stop-at-first-non-option?
            (return (append (reverse processed) args))
            (loop (cdr args) (cons A processed)))))))



;;  Check that all the rules inherent in the /specs/ are fulfilled by
;;  the /options/.
(define (verify-specs-fullfilled specs options)
  (for-each
    (λ (spec)
      (let* ((name (option-spec->name spec))
             (value (assq-ref options (string->symbol name))))
        (when (and (option-spec->required? spec) (not value))
          (fatal-error "option must be specified: --~a" name))
        (let ((policy (option-spec->value-policy spec)))
          (when (and (eq? policy #t) (eq? value  #t))
            (fatal-error "option must be specified with argument: --~a" name))
          (when (and (eq? policy #f) (string? value))
            (fatal-error "option does not support argument: --~a" name)))
        (let ((pred (option-spec->predicate spec)))
          (when (and pred (string? value) (not (pred value)))
            (fatal-error "option predicate failed: --~a" name)))))
   specs))



;;  Check that all the options are matched by a specification.
(define (verify-options options specs)
  (for-each
    (λ (value)
      (unless (or (null? (car value))
                  (find-spec-long specs (symbol->string (car value))))
        (fatal-error "no such option: --~a" (car value))))
   options))



;;  This procedure will simply return if the options and the specs
;;  conform with each other, or else will bail out with an error
;;  message.
(define (check-compliance options specs)
  (verify-specs-fullfilled specs options)
  (verify-options options specs))



(define full-option-re (make-regexp "^--([^=]+)=(.+)?$"))

;; The /normal-args/ are a normalized command line in which all
;; options are expressed long-form, and the task here is to construct an
;; /options/ object: an associative array of option names onto values
;; (or #t if there is no value).
(define (extract-options normal-args stop-at-first-non-option?)
  (let loop ((args normal-args)
             (options '())
             (non-options '()))
       (cond
         ((null? args) (acons '() (reverse non-options) options))
         ((string=? (car args) "--")
          (acons '() (append (reverse non-options) (cdr args)) options))
         ((regexp-exec full-option-re (car args))
          => (λ (match)
                (loop (cdr args)
                      (acons (string->symbol (match:substring match 1))
                             (or (match:substring match 2) #t)
                             options)
                      non-options)))
         (stop-at-first-non-option?
          (acons '() (append (reverse non-options) args) options))
         (else
           (loop (cdr args) options (cons (car args) non-options))))))



(define* (getopt-long program-arguments option-desc-list
                      #:key stop-at-first-non-option)
  "- Scheme Procedure: getopt-long PROGRAM-ARGUMENTS OPTION-DESC-LIST
                                 [#:stop-at-first-non-option]

Process options, handling both long and short options, similar to
the glibc function 'getopt_long'.  PROGRAM-ARGUMENTS should be a value
similar to what (program-arguments) returns.  OPTION-DESC-LIST is a
list of option descriptions.  Each option description must satisfy the
following grammar:

    <option-spec>           :: (<name> . <attribute-ls>)
    <attribute-ls>          :: (<attribute> . <attribute-ls>)
                               | ()
    <attribute>             :: <required-attribute>
                               | <arg-required-attribute>
                               | <single-char-attribute>
                               | <predicate-attribute>
                               | <value-attribute>
    <required-attribute>    :: (required? <boolean>)
    <single-char-attribute> :: (single-char <char>)
    <value-attribute>       :: (value #t)
                               (value #f)
                               (value optional)
    <predicate-attribute>   :: (predicate <1-ary-function>)

    The procedure returns an alist of option names and values.  Each
option name is a symbol.  The option value will be '#t' if no value
was specified.  There is a special item in the returned alist with a
key of the empty list, (): the list of arguments that are not options
or option values.
    By default, options are not required, and option values are not
required.  By default, single character equivalents are not supported;
if you want to allow the user to use single character options, you need
to add a ‘single-char’ clause to the option description."
  (parameterize ((program-name (car program-arguments)))
    (let* ((specs (map parse-option-spec option-desc-list))
           (options (extract-options
                      (normalize (cdr program-arguments)
                                 specs
                                 stop-at-first-non-option)
                      stop-at-first-non-option)))
      (check-compliance options specs)
      options)))



(define (option-ref options key default)
  "Return value in OPTIONS (as returned from getopt-long), using KEY--
a symbol--, or DEFAULT if not found.  The value is either a string or
‘#t’, or whatever DEFAULT is."
  (or (assq-ref options key) default))


;;; getopt-long.scm ends here
