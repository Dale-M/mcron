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



;; This file provides methods for reading a complete Vixie-style configuration
;; file, either from a real file or an already opened port. It also exposes the
;; method for parsing the time-specification part of a Vixie string, so that
;; these can be used to form the next-time-function of a job in a Guile
;; configuration file.



(use-modules (ice-9 regex) (ice-9 rdelim) (srfi srfi-13) (srfi srfi-14))



;; In Vixie-style time specifications three-letter symbols are allowed to stand
;; for the numbers corresponding to months and days of the week. We deal with
;; this by making a textual substitution early on in the processing of the
;; strings.
;;
;; We start by defining, once and for all, a list of cons cells consisting of
;; regexps which will match the symbols - which allow an arbitrary number of
;; other letters to appear after them (so that the user can optionally complete
;; the month and day names; this is an extension of Vixie) - and the value which
;; is to replace the symbol.
;;
;; The procedure then takes a string, and then for each symbol in the
;; parse-symbols list attempts to locate an instance and replace it with an
;; ASCII representation of the value it stands for. The procedure returns the
;; modified string. (Note that each symbol can appear only once, which meets the
;; Vixie specifications technically but still allows silly users to mess things
;; up).

(define parse-symbols
  (map (lambda (symbol-cell)
         (cons (make-regexp (string-append (car symbol-cell) "[[:alpha:]]*")
                            regexp/icase)
               (cdr symbol-cell)))
       '(("jan" . "0")  ("feb" . "1")  ("mar" . "2")  ("apr" . "3")
         ("may" . "4")  ("jun" . "5")  ("jul" . "6")  ("aug" . "7")
         ("sep" . "8")  ("oct" . "9")  ("nov" . "10") ("dec" . "11")
         
         ("sun" . "0")  ("mon" . "1")  ("tue" . "2")  ("wed" . "3")
         ("thu" . "4")  ("fri" . "5")  ("sat" . "6")  )))

(define (vixie-substitute-parse-symbols string)
  (for-each (lambda (symbol-cell)
              (let ((match (regexp-exec (car symbol-cell) string)))
                (if match
                    (set! string (string-append (match:prefix match)
                                                (cdr symbol-cell)
                                                (match:suffix match))))))
            parse-symbols)
  string)



;; A Vixie time specification is made up of a space-separated list of elements,
;; and the elements consist of a comma-separated list of subelements. The
;; procedure below takes a string holding a subelement, which should have no
;; spaces or symbols (see above) in it, and returns a list of all values which
;; that subelement indicates. There are five distinct cases which must be dealt
;; with: [1] a single '*' which returns a list of all values; [2] a '*' followed
;; by a step specifier; [3] a range and step specifier; [4] a range; and [5] a
;; single number.
;;
;; To perform the computation required for the '*' cases, we need to pass the
;; limit of the allowable range for this subelement as the third argument. As
;; days of the month start at 1 while all the other time components start at 0,
;; we must pass the base of the range to deal with this case also.

(define parse-vixie-subelement-regexp
  (make-regexp "^([[:digit:]]+)(-([[:digit:]]+)(/([[:digit:]]+))?)?$"))

(define (parse-vixie-subelement string base limit)
  (if (char=? (string-ref string 0) #\*)
      (range base limit (if (> (string-length string) 1)
                            (string->number (substring string 2))  ;; [2]
                            1))  ;; [1]
      (let ((match (regexp-exec parse-vixie-subelement-regexp string)))
        (cond ((not match)
               (display "Error: Bad Vixie-style time specification.\n")
               (primitive-exit 9))
              ((match:substring match 5)
               (range (string->number (match:substring match 1))
                      (+ 1 (string->number (match:substring match 3)))
                      (string->number (match:substring match 5))))  ;; [3]
              ((match:substring match 3)
               (range (string->number (match:substring match 1))
                      (+ 1 (string->number (match:substring match 3))))) ;; [4]
              (else
               (list (string->number (match:substring match 1))))))))  ;; [5]



;; A Vixie element contains the entire specification, without spaces or symbols,
;; of the acceptable values for one of the time components (minutes, hours,
;; days, months, week days). Here we break the comma-separated list into
;; subelements, and process each with the procedure above. The return value is a
;; list of all the valid values of all the subcomponents.
;;
;; The second and third arguments are the base and upper limit on the values
;; that can be accepted for this time element.
;;
;; The effect of the 'apply append' is to merge a list of lists into a single
;; list.

(define (parse-vixie-element string base limit)
  (apply append
   (map (lambda (sub-element)
                (parse-vixie-subelement sub-element base limit))
        (string-tokenize string (char-set-complement (char-set #\,))))))



;; Consider there are two lists, one of days in the month, the other of days in
;; the week. This procedure returns an augmented list of days in the month with
;; weekdays accounted for.

(define (interpolate-weekdays mday-list wday-list month year)
  (let ((t (localtime 0)))
    (set-tm:mday  t 1)
    (set-tm:mon t month)
    (set-tm:year  t year)
    (let ((first-day (tm:wday (cdr (mktime t)))))
      (apply append
             mday-list
             (map (lambda (wday)
                    (let ((first (- wday first-day)))
                      (if (< first 0) (set! first (+ first 7)))
                      (range (+ 1 first) 32 7)))
                  wday-list)))))



;; Return the number of days in a month. Fix up a tm object for the zero'th day
;; of the next month, rationalize the object and extract the day.

(define (days-in-month month year)
  (let ((t (localtime 0))) (set-tm:mday  t 0)
                           (set-tm:mon t (+ month 1))
                           (set-tm:year  t year)
                           (tm:mday (cdr (mktime t)))))



;; We will be working with a list of time-spec's, one for each element of a time
;; specification (minute, hour, ...). Each time-spec holds three pieces of
;; information: a list of acceptable values for this time component, a procedure
;; to get the component from a tm object, and a procedure to set the component
;; in a tm object.

(define (time-spec:list    time-spec) (vector-ref time-spec 0))
(define (time-spec:getter  time-spec) (vector-ref time-spec 1))
(define (time-spec:setter  time-spec) (vector-ref time-spec 2))



;; This procedure modifies the time tm object by setting the component referred
;; to by the time-spec object to its next acceptable value. If this value is not
;; greater than the original (because we have wrapped around the top of the
;; acceptable values list), then the function returns #t, otherwise it returns
;; #f. Thus, if the return value is true then it will be necessary for the
;; caller to increment the next coarser time component as well.
;;
;; The first part of the let block is a concession to humanity; the procedure is
;; simply unreadable without all of these aliases.

(define (increment-time-component time time-spec)
  (let* ((time-list   (time-spec:list   time-spec))
         (getter      (time-spec:getter time-spec))
         (setter      (time-spec:setter time-spec))
         (next-best   (find-best-next (getter time) time-list))
         (wrap-around (eqv? (cdr next-best) 9999)))
    (setter time ((if wrap-around car cdr) next-best))
    wrap-around))



;; There now follows a set of procedures for adjusting an element of time,
;; i.e. taking it to the next acceptable value. In each case, the head of the
;; time-spec-list is expected to correspond to the component of time in
;; question. If the adjusted value wraps around its allowed range, then the next
;; biggest element of time must be adjusted, and so on.

;;   There is no specification allowed for the year component of
;;   time. Therefore, if we have to make an adjustment (presumably because a
;;   monthly adjustment has wrapped around the top of its range) we can simply
;;   go to the next year.

(define (nudge-year! time)
  (set-tm:year time (+ (tm:year time) 1)))


;;   We nudge the month by finding the next allowable value, and if it wraps
;;   around we also nudge the year. The time-spec-list will have time-spec
;;   objects for month and weekday.

(define (nudge-month! time time-spec-list)
  (and (increment-time-component time (car time-spec-list))
       (nudge-year! time)))


;;   Try to increment the day component of the time according to the combination
;;   of the mday-list and the wday-list. If this wraps around the range, or if
;;   this falls outside the current month (31st February, for example), then
;;   bump the month, set the day to zero, and recurse on this procedure to find
;;   the next day in the new month.
;;
;;   The time-spec-list will have time-spec entries for mday, month, and
;;   weekday.

(define (nudge-day! time time-spec-list)
  (if (or (increment-time-component
              time
              (vector 
               (interpolate-weekdays (time-spec:list (car time-spec-list))
                                     (time-spec:list (caddr time-spec-list))
                                     (tm:mon time)
                                     (tm:year time))
               tm:mday
               set-tm:mday))
          (> (tm:mday time) (days-in-month (tm:mon time) (tm:year time))))
      (begin
        (nudge-month! time (cdr time-spec-list))
        (set-tm:mday time 0)
        (nudge-day! time time-spec-list))))



;;   The hour is bumped to the next accceptable value, and the day is bumped if
;;   the hour wraps around.
;;
;;   The time-spec-list holds specifications for hour, mday, month and weekday.

(define (nudge-hour! time time-spec-list)
  (and (increment-time-component time (car time-spec-list))
       (nudge-day! time (cdr time-spec-list))))



;;   The minute is bumped to the next accceptable value, and the hour is bumped
;;   if the minute wraps around.
;;
;;   The time-spec-list holds specifications for minute, hour, day-date, month
;;   and weekday.

(define (nudge-min! time time-spec-list)
  (and (increment-time-component time (car time-spec-list))
       (nudge-hour! time (cdr time-spec-list))))




;; This is a procedure which returns a procedure which computes the next time a
;; command should run after the current time, based on the information in the
;; Vixie-style time specification.
;;
;; We start by computing a list of time-spec objects (described above) for the
;; minute, hour, date, month, year and weekday components of the overall time
;; specification [1]. When we create the return procedure, it is this list to
;; which references to a time-spec-list will be bound. It will be used by the
;; returned procedure [3] to compute the next time a function should run. Any
;; 7's in the weekday component of the list (the last one) are folded into 0's
;; (both values represent sunday) [2].
;;
;; The returned procedure itself:-
;;
;;   Starts by obtaining the current broken-down time [4], and fixing it to
;;   ensure that it is an acceptable value, as follows. Each component from the
;;   biggest down is checked for acceptability, and if it is not acceptable it
;;   is bumped to the next acceptable value (this may cause higher components to
;;   also be bumped if there is range wrap-around) and all the lower components
;;   are set to -1 so that it can successfully be bumped up to zero if this is
;;   an allowed value. The -1 value will be bumped up subsequently to an allowed
;;   value [5].
;;
;;   Once it has been asserted that the current time is acceptable, or has been
;;   adjusted to one minute before the next acceptable time, the minute
;;   component is then bumped to the next acceptable time, which may ripple
;;   through the higher components if necessary [6]. We now have the next time
;;   the command needs to run.
;;
;;   The new time is then converted back into a UNIX time, and returned [7].

(define (parse-vixie-time string)
  (let* ((tokens (string-tokenize (vixie-substitute-parse-symbols string)))
         (time-spec-list
          (map-in-order (lambda (x) (vector (parse-vixie-element
                                              (list-ref tokens (vector-ref x 0))
                                            (vector-ref x 1)
                                            (vector-ref x 2))
                                   (vector-ref x 3)
                                   (vector-ref x 4)))
                    ;; token range-top+1   getter    setter
                 `( #( 0     0     60      ,tm:min   ,set-tm:min   )
                    #( 1     0     24      ,tm:hour  ,set-tm:hour  )
                    #( 2     1     32      ,tm:mday  ,set-tm:mday  )
                    #( 3     0     12      ,tm:mon   ,set-tm:mon   )
                    #( 4     0      7      ,tm:wday  ,set-tm:wday  ))))) ;; [1]

    (vector-set! (car (last-pair time-spec-list))
                 0
                 (map (lambda (time-spec)
                        (if (eqv? time-spec 7) 0 time-spec))
                      (vector-ref (car (last-pair time-spec-list)) 0)))  ;; [2]
    
    (lambda (current-time)     ;; [3]
      (let ((time (localtime current-time)))  ;; [4]
        
        (if (not (member (tm:mon time)
                         (time-spec:list (cadddr time-spec-list))))
            (begin
              (nudge-month! time (cdddr time-spec-list))
              (set-tm:mday  time 0)
              (set-tm:hour time -1)
              (set-tm:min  time -1)))
        (if (not (member (tm:mday time)  ;; !!
                         (time-spec:list (caddr time-spec-list))))
            (begin
              (nudge-day! time (cddr time-spec-list))
              (set-tm:hour time -1)
              (set-tm:min time -1)))
        (if (not (member (tm:hour time)
                         (time-spec:list (cadr time-spec-list))))
            (begin
              (nudge-hour! time (cdr time-spec-list))
              (set-tm:min time -1)))   ;; [5]

        (set-tm:sec time 0)
        (nudge-min! time time-spec-list)  ;; [6]

        (car (mktime time))))))   ;; [7]




;; A line in a Vixie-style crontab file which gives a command specification
;; carries two pieces of information: a time specification consisting of five
;; space-separated items, and a command which is also separated from the time
;; specification by a space. The line is broken into the two components, and the
;; job procedure run to add the two pieces of information to the job list (this
;; will in turn use the above function to turn the time specification into a
;; function for computing future run times of the command).

(define parse-user-vixie-line-regexp
  (make-regexp "^[[:space:]]*(([^[:space:]]+[[:space:]]+){5})(.*)$"))

(define (parse-user-vixie-line line)
  (let ((match (regexp-exec parse-user-vixie-line-regexp line)))
    (if (not match) (begin (display "Bad job line in Vixie file.\n")
                           (primitive-exit 10)))
    (job (match:substring match 1)
         (lambda () (with-mail-out (match:substring match 3))))))



;; The case of reading a line from /etc/crontab is similar to above but the user
;; ID appears in the sixth field, before the action.

(define parse-system-vixie-line-regexp
  (make-regexp (string-append "^[[:space:]]*(([^[:space:]]+[[:space:]]+){5})"
                              "([[:alpha:]][[:alnum:]_]*)[[:space:]]+(.*)$")))

(define (parse-system-vixie-line line)
  (let ((match (regexp-exec parse-user-vixie-line-regexp line)))
    (if (not match) (begin (display "Bad job line in /etc/crontab.\n")
                           (primitive-exit 11)))
    (set! configuration-user (passwd (match:substring match 3)))
    (job (match:substring match 1)
         (lambda () (with-mail-out (match:substring match 4)
                                   (passwd:name configuration-user))))))




;; The next procedure reads an entire Vixie-style file. For each line in the
;; file there are three possibilities (after continuation lines have been
;; appended): the line is blank or contains only a comment, the line contains an
;; environment modifier which will be handled in environment.scm, or the line
;; contains a command specification in which case we use the procedure above to
;; add an entry to the internal job list.
;;
;; Note that the environment modifications are cleared, so that there is no
;; interference between crontab files (this might lead to unpredictable
;; behaviour because the order in which crontab files are processed, if there is
;; more than one, is generally undefined).

(define read-vixie-file-comment-regexp
  (make-regexp "^[[:space:]]*(#.*)?$"))


(define (read-vixie-port port . parse-vixie-line)
  (clear-environment-mods)
  (if port
      (let ((parse-vixie-line
             (if (null? parse-vixie-line) parse-user-vixie-line
                 (car parse-vixie-line))))
        (do ((line (read-line port) (read-line port)))
            ((eof-object? line))
          
          ;; If the line ends with \, append the next line.
          (do ()
              ((or (< (string-length line) 1)
                   (not (char=? (string-ref line
                                            (- (string-length line) 1))
                                #\\))))
            (let ((next-line (read-line port)))
              (if (eof-object? next-line)
                  (set! next-line ""))
              (set! line
                    (string-append
                     (substring line 0 (- (string-length line) 1))
                     next-line))))

          ;; Consider the three cases mentioned in the description.
          (or (regexp-exec read-vixie-file-comment-regexp line)
              (parse-vixie-environment line)
              (parse-vixie-line line))))))



;; If a file cannot be opened, we must silently ignore it because it may have
;; been removed by crontab. However, if the file is there it must be parseable,
;; otherwise the error must be propagated to the caller.

(define (read-vixie-file file-path . parse-vixie-line)
  (let ((port #f))
    (catch #t (lambda () (set! port (open-input-file file-path)))
           (lambda (key . args) (set! port #f)))
    (if port
        (begin
          (if (null? parse-vixie-line)
              (read-vixie-port port)
              (read-vixie-port port (car parse-vixie-line)))
          (close port)))))
