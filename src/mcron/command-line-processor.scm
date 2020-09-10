;;;;                                                            -*- scheme -*-
;;;; command-line-processor.scm --- command-line options processing
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

;;; Author: Dale Mellor   May, 2020

;;; Commentary:

;;; Where the Guile (ice-9 getopt-long) module, modelled after the GNU C
;;; libraryʼs ‘getopt_long’ function, allows an application to construct
;;; a grammar prescribing the decomposition of the command-line options,
;;; this module, inspired by the C libraryʼs ‘argp’ parser, gives the
;;; application a higher-level paradigm in which the command-line
;;; processing is specified declaratively.  This includes enough of the
;;; application meta-data and some fragmentary help strings for the
;;; completely automatic generation of responses to GNU-standard
;;; ‘--help’, ‘--version’ and ‘--usage’ options, thus alleviating the
;;; need of the application itself to deal with these things.
;;;
;;; The module has three specific aims.
;;;
;;;    1) Provide higher-level declarative interface, easier to use.
;;;
;;;    2) Automatically respond to --help, --version and --usage
;;;       options.
;;;
;;;    3) Allow amalgamation of specifications, so that an application
;;;       can mix in requirements from modules into its own option
;;;       specification--THIS IS NOT CURRENTLY IMPLEMENTED.
;;;
;;; There is just one function which needs to be called to get all of
;;; this functionality: it is ‘process-command-line’, and has the side
;;; effect that new variable bindings appear in the current module
;;; corresponding to all the options.  For example, if a declared option
;;; is ‘--do-this’, then a variable called, literally, ‘--do-this’ will
;;; be injected in the current namespace and will have the value
;;; provided on the command-line, or simply #t or #f to indicate whether
;;; or not that option was present on the command line.
;;;
;;; Alternatively, it is possible to create and compose the
;;; specification in separate steps, and then call the above method with
;;; the results.  The functions ‘command-line-specification’ and
;;; ‘merge-command-line-specifications’ are provided to this end.

;;; (process-command-line  COMMAND-LINE  SPECIFICATION)
;;; Process the COMMAND-LINE according to the application SPECIFICATION.
;;;
;;; COMMAND-LINE is a list of strings, such as that returned from the
;;; core ‘command-line’ function.
;;;
;;; SPECIFICATION is a form holding a space-separated mix of selection
;;; words followed by their respective declarations.  The selection
;;; words are ‘application’, ‘author’, ‘bug-address’, ‘copyright’,
;;; ‘help-preamble’, ‘help-postamble’, ‘license’, ‘option’, ‘usage’ and
;;; ‘version’, and can appear in any order.
;;;
;;;    ‘application’ should be followed by a string: the name of the
;;;           application with possibly the package name in
;;;           parentheses afterwards
;;;    ‘author’ should be followed by a string giving the name of one of
;;;           the packageʼs authors.  This selection word can be
;;;           repeated as many times as necessary to provide the names
;;;           of all authors.
;;;    ‘bug-address’ should be followed by a string giving the URL of a
;;;           contact-point for sending bug reports, such as an
;;;           e-mail address or web address of bug-tracking system
;;;           interface
;;;    ‘copyright’ should be followed by a string containing a list of
;;;           years and an entity to whom the copyright is assigned.
;;;           This may be repeated to list other assignees
;;;    ‘help-preamble’ should be followed by a number of strings which
;;;           make up a short paragraph of text displayed before
;;;           a full list of the available program options
;;;    ‘help-postamble’, like the preamble, is followed by strings which
;;;           make up a paragraph of text, shown after the list
;;;           of options
;;;    ‘license’ can be followed by one of the words ‘GPLv3’ [this is
;;;           currently the only standard choice implemented], or else
;;;           a string which briefly gives out the terms of the license
;;;    ‘option’ is followed by an option declaration, described below
;;;    ‘usage’ is followed by a string describing the usage of the
;;;           application on one line
;;;    ‘version’ is followed by a string providing the current version
;;;           number of this program
;;;
;;; The ‘option’ declaration is followed by another form bracketed by
;;; parentheses and holding a space-separated mix of declarations (order
;;; irrelevant).
;;;
;;;    A word beginning with two hyphens, an optional exclamation point,
;;;    alpha-numeric characters, an optional equals sign, and an
;;;    optional further word.  There must be exactly one of these, and
;;;    they determine the long name of the option.  An exclamation point
;;;    indicates that the option MUST appear on the command line, an
;;;    equals indicates that the option MUST have a value unless it is
;;;    followed in the specification by a value, in which case the value
;;;    on the command-line is optional and the one in the specification
;;;    will be taken as the default when not given on the command line.
;;;
;;;    A word comprised of one hyphen and one letter or number.  There
;;;    can be exactly zero or one of these, and it declares that the
;;;    option has this short form available on the command-line.  As a
;;;    very special exception: if you want to use ‘-i’ as an option, it
;;;    must be specified with the identifier ‘short-i’ (a naked /-i/ is
;;;    read as a complex number); ditto ‘short-I’ for ‘-I’.
;;;
;;;    A number of strings which are catenated together to provide a
;;;    short, succinct description of the option.  These strings should
;;;    be approximately half the width of a page, i.e. about 40
;;;    characters.
;;;
;;;    A function which will be used as a predicate to decide if a value
;;;    is allowable for this option.  There should be zero or one of
;;;    these.
;;;
;;; For the precise presentation of options on the command-line, the
;;; reader should refer to the description of the ‘getopt-long’ module,
;;; which underlies the present one.
;;;
;;; At this point a short example is in order.  The main entry point for
;;; the GNU Mcron program has as its first clause
;;;
;;; (process-command-line  (command-line)
;;;       application   "mcron"
;;;       version       "1.4"
;;;       usage         "[OPTIONS]... [FILES]..."
;;;       help-preamble
;;;  "Run an mcron process according to the specifications in the FILE... "
;;;  "(`-' for standard input), or use all the files in ~/.config/cron "
;;;  "(or the deprecated ~/.cron) with .guile or .vixie extensions.\n"
;;;  "Note that --daemon and --schedule are mutually exclusive."
;;;       option  (--daemon  -d
;;;                      "run as a daemon process")
;;;       option  (--stdin=guile  short-i  (λ (in) (or (string=? in "guile")
;;;                                                    (string=? in "vixie")))
;;;                      "format of data passed as standard input or file "
;;;                      "arguments, 'guile' or 'vixie' (default guile)")
;;;       option  (--schedule=8  -s  string->number
;;;                      "display the next N (or 8) jobs that will be run")
;;;       help-postamble
;;;  "Mandatory or optional arguments to long options are also mandatory or "
;;;  "optional for any corresponding short options."
;;;       bug-address "bug-mcron@gnu.org"
;;;       copyright   "2003, 2006, 2014, 2020  Free Software Foundation, Inc."
;;;       license     GPLv3)
;;;
;;; after which there are four new variable bindings in the present
;;; namespace: --daemon, --stdin, --schedule and --! (the latter holds
;;; all the command-line arguments that did not partake in option
;;; processing) whose values depend on the specific command-line options
;;; the end user furnished.

;;; (command-line-specification  SPECIFICATION)
;;; Compiles an object which encapsulates the given SPECIFICATION.
;;;
;;; For details of how to give a SPECIFICATION, see the description of
;;; the full ‘process-command-line’ function above.  The return from
;;; this method can be used in the partial version of
;;; ‘process-command-line’ described below, and in the following
;;; ‘merge-command-line-specifications’ function.

;;; (merge-command-line-specifications SPECIFICATION_OBJECT ...)  Make a
;;; single specification object which embodies the amalgamation of all
;;; of the specification objects given as arguments.
;;;
;;; Order is important: if two option items specify the same short form
;;; for the option (a single letter), then only the first option will
;;; actually have that short form available at the command-line.
;;; Similarly, if two options have exactly the same name, the second (or
;;; later) ones will have a numerical digit appended to their name.

;;; (process-command-line COMMAND-LINE SPECIFICATION-OBJECT) Perform
;;; exactly the same function as the full ‘process-command-line’
;;; function described above, but takes a pre-made specification object
;;; produced using the two functions above.

;;; Bugs/To do
;;;
;;; 1) This stuff currently only works in the top-level module.
;;;
;;; 2) Want to be able to amalgamate command-line specifications from
;;; different modules.  Will need to get to the bottom of the first
;;; issue before we can tackle this one (somehow need to put the
;;; --option variable bindings into the right places, or at least
;;; replicate them all in all modules which want to do some processing
;;; of the command line).
;;;
;;; 3) Want more license boilerplate text; currently we only have GPLv3.

;;; Code:

(define-module (mcron command-line-processor)
  #:use-module (srfi srfi-1)       ;; fold
  #:use-module (srfi srfi-9)       ;; records
  #:use-module (srfi srfi-9 gnu)   ;; set/get-fields
  #:use-module (mcron getopt-long)
  #:use-module (ice-9 regex)
  #:export (specific option item
            obtain-getopt-long-results
            process-getopt-long-results
            
            ;; These are the real public exports.
            process-command-line
            command-line-specification
            merge-command-line-specifications))



(define-record-type  <<specification>>
       (make-specification- preamble postamble copyright authors options)
       specification?
          (name        spec:name      spec:set-name!)
          (version     spec:version   spec:set-version!)
          (usage       spec:usage     spec:set-usage!)
          (preamble    spec:preamble  spec:set-preamble!)
          (postamble   spec:postamble spec:set-postamble!)
          (bug-address spec:bugs      spec:set-bugs!)
          (copyright   spec:copyright spec:set-copyright!)
          (license     spec:license   spec:set-license!)
          (authors     spec:authors   spec:set-authors!)
          (options     spec:options   spec:set-all-options!))

;; We initialize the fields which are supposed to be lists, but
;; generally this procedure should *not* be considered to be producing a
;; properly specified <<specification>> record.
(define (make-specification) (make-specification- '() '() '() '() '()))



(define-record-type  <<option>>
       (make-option- description)
       option?
          (name         option:name         option:set-name!)
          (required?    option:required?)
          (short-letter option:short        option:set-short!)
          (value?       option:value?)
          (default      option:default)
          (description  option:description  option:set-description!)
          (predicate    option:predicate    option:set-predicate!))

;; As above, this initializer does *not* return a properly defined
;; object.
(define (make-option) (make-option- '()))



(define (has-option-short-form spec letter)
  (not (or (not letter)
           (eq? #f (find (λ (a) (eqv? letter (option:short a)))
                         (spec:options spec))))))

(define (has-option-name spec name)
  (not (eq? #f (find (λ (a) (string=? (option:name a) name))
                     (spec:options spec)))))

(define (merge-command-line-specifications- A B)
  (for-each (λ (b-option)
               (when (has-option-short-form A (option:short b-option))
                 (option:set-short!  b-option  #f))
               (let ((base-name (option:name b-option)))
                 (when (has-option-name A base-name)
                   (let loop ((count 1))
                        (let ((new-name (string-append base-name "-"
                                                       (number->string count))))
                          (if (has-option-name A new-name)
                              (loop (1+ count))
                              (option:set-name!  b-option  new-name))))))
               (spec:set-all-options! A (append (spec:options A)
                                                (list b-option))))
            (spec:options B))
  A)

(define-syntax merge-command-line-specifications
  (syntax-rules ()
  "- Scheme Procedure: merge-command-line-specifications A B ...

Append the list of options in A with those in B, but drop any
short-forms in B which clash with existing ones, and if a long option
name clashes then append a number to make it unique.  All of the
arguments will be mutilated in the process, and a new specification
object will be returned."
    ((_ A B) (merge-command-line-specifications- A B))
    ((_ A B . C) (merge-command-line-specifications
                   (merge-command-line-specifications- A B)
                   . C))))



(define  long-re   (make-regexp "^--(!)?([[:alnum:]][-_[:alnum:]]*)(=(.+)?)?$"))
(define  short-re  (make-regexp  "^-[[:alnum:]]$"))


(define-syntax item   ;; As in, an option item (long name, short form...).
  (λ (x) (syntax-case x (short-i short-I)

    ;; No more work to do.
    ((_ O) #'#t)

    ;; Next option is a string: take as description.
    ((_ O desc . args)
     (string? (syntax->datum #'desc))
     #'(begin (option:set-description! O (append (option:description O)
                                                 (list desc)))
              (item O . args)))

    ;; Next option is short-form.
    ((_ O short-i . args)
     #'(begin (option:set-short! O #\i)
              (item O . args)))

    ((_ O short-I . args)
     #'(begin (option:set-short! O #\I)
              (item O . args)))

    ((_ O short . args)
     (and (identifier? #'short)
          (regexp-exec short-re (symbol->string (syntax->datum #'short))))
     #'(begin (option:set-short! O (string-ref (symbol->string 'short) 1))
              (item O . args)))

    ;; Next option is long-form.
    ((_ O long . args)
     (and (identifier? #'long)
          (regexp-exec long-re (symbol->string (syntax->datum #'long))))
     #'(begin (let ((match (regexp-exec long-re
                                (symbol->string (syntax->datum #'long)))))
                (set! O
                   (set-fields O
                     ((option:name)       (match:substring match 2))
                     ((option:required?)  (if (match:substring match 1) #t #f))
                     ((option:value?)
                                 (cond ((not (match:substring match 3)) #f)
                                       ((match:substring match 4)  'optional)
                                       (else #t)))
                     ((option:default)    (match:substring match 4)))))
              (item O . args)))

    ;; Next option is a procedure: take as predicate.

    ((_ O (lambda args ...) . Args)
     #'(begin (option:set-predicate! O (lambda args ...))
              (item O . Args)))

    ((_ O pred . args)
     (and (identifier? #'pred)
          ;; (procedure? (primitive-eval (syntax->datum #'pred)))
          )
     #'(begin (option:set-predicate! O pred)
              (item O . args))))))



(define-syntax-rule  (option args ...)
    (let ((O (make-option))) (item O args ...) O))



(define-syntax  specific
  (λ (x)  (syntax-case  x  (application  author         bug-address
                            copyright    help-preamble  help-postamble
                            license      option         usage
                            version)
     ((specific spec application A args ...)
      (string? (syntax->datum #'A))
            #'(begin (spec:set-name! spec A)
                     (specific spec args ...)))
     ((specific spec author A args ...)
      (string? (syntax->datum #'A))
            #'(begin (spec:set-author! spec (append (spec:authors spec)
                                                    (list A)))
                     (specific spec args ...)))
     ((specific spec bug-address B args ...)
      (string? (syntax->datum #'B))
            #'(begin (spec:set-bugs! spec B)
                     (specific spec args ...)))
     ((specific spec copyright C args ...)
      (string? (syntax->datum #'C))
            #'(begin (spec:set-copyright! spec (append (spec:copyright spec)
                                                       (list C)))
                      (specific spec args ...)))
     ((specific spec help-preamble id args ...)
      (identifier? #'id)
            #'(specific spec id args ...))
     ((specific spec help-preamble quotation args ...)
      (string? (syntax->datum #'quotation))
            #'(begin (spec:set-preamble! spec (append (spec:preamble spec)
                                                      (list quotation)))
                     (specific spec help-preamble args ...)))
     ((specific spec help-postamble id args ...)
      (identifier? #'id)
            #'(specific spec id args ...))
     ((specific spec help-postamble quotation args ...)
      (string? (syntax->datum #'quotation))
            #'(begin (spec:set-postamble! spec (append (spec:postamble spec)
                                                       (list quotation)))
                     (specific spec help-postamble args ...)))
     ((specific spec license L args ...)
      (identifier? #'L)
            #'(begin (spec:set-license! spec 'L)
                     (specific spec args ...)))
     ((specific spec license L args ...)
      (string? (syntax->datum #'L))
            #'(begin (spec:set-license! spec L)
                     (specific spec args ...)))
     ((specific spec option (args ...) Args ...)
            #'(begin (spec:set-all-options! spec
                                            (append (spec:options spec)
                                                    (list (option args ...))))
                     (specific spec Args ...)))
     ((specific spec usage U args ...)
      (string? (syntax->datum #'U))
            #'(begin (spec:set-usage! spec U)
                     (specific spec args ...)))
     ((specific spec version V args ...)
      (string? (syntax->datum #'V))
            #'(begin (spec:set-version! spec V)
                     (specific spec args ...)))
     ((specific spec)  #'#t))))



(define-syntax-rule  (command-line-specification args ...)
;;   " - Scheme Procedure: command-line-specification ARGS ...

;; Furnish an application specification object with attributes specified in
;; ARGS followed by a number of values for the attribute.  Please refer to
;; full documentation for a proper description of a specification object.

;; The attributes are

;;  application: string: the formal name of this application.  Must appear
;;          exactly once.
;;  author: string: the name of an author.  May appear any number of times.
;;  bug-address: string: The URI to which bug reports should be addressed.
;;          May appear zero or one times.
;;  copyright: string: list of years and owning entity.  May appear any
;;          number of times.
;;  help-preamble: string: text to precede the list of options in a
;;          response to the --help option.  This attribute may appear any
;;          number of times, and each occurrence can be followed by one or
;;          more strings which will be assembled together into paragraphs.
;;  help-postamble: string: text to succeed the list of options in a help
;;          message.  Same considerations apply as to ‘help-preamble’.
;;  license: identifier or string: either the identifier ‘GPLv3’ or a
;;          string describing the terms of the license.
;;  option: (sub-form): the sub-form must contain one identifier composed
;;          of two hyphens, an optional exclamation point, a token of
;;          letters, numbers, underscore and hyphen, an optional equals
;;          sign, and an optional word; the sub-form may have zero or one
;;          identifiers composed of a hyphen and a single letter; any
;;          number of strings which will be composed into a paragraph of
;;          help for the option (these should be sized to half-line
;;          lengths); and zero or one procedures which will be applied as a
;;          predicate on allowable option values.  Any number of these
;;          option attributes may appear in the specification.
;;  usage: string: a single line of text prototyping the command line.
;;          Zero or one of these may appear.
;;  version: string: the version number of this application.  Zero or one
;;          of these attributes may appear."

  (let ((spec (make-specification)))
    (specific spec args ...)
    spec))



(define (version-string spec)
  (with-output-to-string (λ ()
    (display (if (string? (spec:name spec))
                 (spec:name spec)
                 (car (command-line))))
    (when (string? (spec:version spec))
      (for-each display `(" " ,(spec:version spec) "\n")))
    (unless (null? (spec:copyright spec))
      (for-each display `("Copyright © "
                          ,(string-join (spec:copyright spec) "\n            ")
                          "\n")))
    (cond ((eq? (spec:license spec) 'GPLv3)
                (display (string-append
                          "License GPLv3+: GNU GPL version 3 or later "
                          "<https://gnu.org/licenses/gpl.html>.\nThis is "
                          "free software: you are free to change and "
                          "redistribute it.\nThere is NO WARRANTY, to the "
                          "extent permitted by law.\n")))
          ((string? (spec:license spec))
                (display (spec:license spec)) (newline)))
    (unless (null? (spec:authors spec))
      (display (string-append "Written by "
                        (case (length (spec:authors spec))
                          ((1 2)  (string-join (spec:authors spec) " and "))
                          (else
                           (let loop ((a (cdr (spec:authors spec)))
                                      (ret (car (spec:authors spec))))
                                (if (null? (cdr a))
                                    (string-append ret " and " (car a))
                                  (loop (cdr a) (string-append ret ", "
                                                               (car a)))))))
                        ".\n"))))))



(define (usage-string spec)
  (string-append "Usage: " (spec:name spec) " " (spec:usage spec) "\n"))



(define  (help-string spec)
  (with-output-to-string (λ ()
    (for-each display `(,(usage-string spec)
                        ,(string-join (spec:preamble spec) "\n")
                        "\n\n"))
    (let ((max-length (fold (λ (o r) (max r (string-length (option:name o))))
                            0
                            (spec:options spec))))
      (for-each (λ (o)
              (display "  ")
              (cond ((option:short o)
                     (display "-") (display (option:short o))
                     (case (option:value? o)  ((#t)        (display "N,   "))
                                              ((optional)  (display "[N], "))
                                              (else        (display ",    "))))
                    (else (display "       ")))
              (display "--")
              (display (option:name o))
              (case (option:value? o)  ((#t)       (display "=N  "))
                                       ((optional) (display "[=N]"))
                                       (else       (display "    ")))
              (display (make-string (- max-length
                                       (string-length (option:name o)))
                                    #\space))
              (display "  ")
              (when  (option:required? o)  (display "*REQUIRED*: "))
              (display (string-join (option:description o)
                                    (string-append "\n"
                                                   (make-string max-length
                                                                #\space)
                                                   "                   ")))
              (newline))
           (spec:options spec)))
     (newline)
     (display (string-join (spec:postamble spec) "\n"))
     (when (spec:bugs spec)
       (for-each display
                 `("\nSend bug reports to " ,(spec:bugs spec) ".\n"))))))



;;  Make a list out of the args, omitting any #f.
(define (compose-list . args)
  (let loop ((ret '()) (args args))
       (cond ((null? args) (reverse ret))
             ((not (car args)) (loop ret (cdr args)))
             (else (loop (cons (car args) ret) (cdr args))))))

(define (make-getopt-long-input spec)
  (map (λ (o)
         (compose-list (string->symbol (option:name o))
                       (and=> (option:short o) (λ (x) `(single-char ,x)))
                       `(required? ,(option:required? o))
                       `(value ,(if (option:default o)
                                    'optional
                                  (option:value? o)))
                       (and=> (option:predicate o) (λ (x) `(predicate ,x)))))
       (spec:options spec)))



(define-syntax-rule (obtain-getopt-long-results args spec)
  (getopt-long args (make-getopt-long-input spec)))



(define  (distill-getopt-long-results  go-l  spec)
  (cons (cons "!" (option-ref go-l '() '()))
        (map (λ (o)
                (let ((g (option-ref go-l
                                     (string->symbol (option:name o))
                                     #f)))
                  (when g
                    (case (string->symbol (option:name o))
                          ((help)    (display (help-string spec))    (exit 0))
                          ((version) (display (version-string spec)) (exit 0))
                          ((usage)   (display (usage-string spec))   (exit 0))))
                  (cons (option:name o)
                        (if (and (eq? #t g)
                                 (not (eq? #f (option:default o))))
                            (option:default o)
                            g))))
             (spec:options spec))))



(define  (process-getopt-long-results go-l spec)
  (for-each (λ (option)
               (module-define!
                        (current-module)
                        (string->symbol (string-append "--" (car option)))
                        (cdr option)))
            (distill-getopt-long-results go-l spec)))



(define-syntax  process-command-line
;;   "- Scheme Procedure: process-command-line COMMAND-LINE SPECS [...]

;; Process the COMMAND-LINE according to the SPECS, extracting options and
;; their values, dealing with --help, --version and --usage requests.  The
;; procedure has no return values, but has the side effect of creating
;; variable bindings in the current module corresponding to the long form
;; of the options, plus a variable called ‘--!’ which gets a list of all
;; the arguments on the command-line which did not participate in option
;; processing.

;; The COMMAND-LINE is a list of strings, starting with the name of the
;; program and containing all the tokens passed to the program on the
;; command line, such as returned from the core ‘command-line’ procedure.

;; The SPECS should have been obtained with the
;; ‘command-line-specification’ procedure, or, as a short-cut, can be
;; supplied directly as arguments to this procedure."

  (syntax-rules ()
    ((_ command-line specs)
         (let ((S (merge-command-line-specifications
                   specs
                   (command-line-specification
                     option (-h --help    "display this help and exit")
                     option (-V --version "output version information and exit")
                     option (-u --usage   "show brief usage summary")))))
           (process-getopt-long-results
             (obtain-getopt-long-results command-line S)
             S)))
    ((_ command-line item ...)
         (process-command-line
              command-line
              (command-line-specification item ...)))))
