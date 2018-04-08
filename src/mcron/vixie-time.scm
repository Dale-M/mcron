;;;; vixie-time.scm -- parse Vixie-style times
;;; Copyright © 2003 Dale Mellor <dale_mellor@users.sourceforge.net>
;;; Copyright © 2018 Mathieu Lirzin <mthl@gnu.org>
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

(define-module (mcron vixie-time)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (mcron job-specifier)
  #:use-module (srfi srfi-1)
  #:export (parse-vixie-time))

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
               (throw 'mcron-error 9 
                      "Bad Vixie-style time specification."))
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

(define (interpolate-weekdays mday-list wday-list month year)
  "Given a list of days in the month MDAY-LIST and a list of days in the week
WDAY-LIST, return an augmented list of days in the month with weekdays
accounted for."
  (let ((t (localtime 0)))
    (set-tm:mday t 1)
    (set-tm:mon t month)
    (set-tm:year t year)
    (let ((first-day (tm:wday (cdr (mktime t)))))
      (define (range-wday wday)
        (let* ((first  (- wday first-day))
               (first* (if (negative? first) (+ 7 first) first)))
          (range (1+ first*) 32 7)))
      (apply append mday-list (map range-wday wday-list)))))

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
  (let ((time-list      (time-spec:list   time-spec))
        (getter         (time-spec:getter time-spec))
        (setter         (time-spec:setter time-spec))
        (find-best-next (@@ (mcron job-specifier) %find-best-next)))
    (match (find-best-next (getter time) time-list)
      ((smallest . closest+)
       (let ((infinite (inf? closest+)))
         (if infinite
             (setter time smallest)
             (setter time closest+))
         infinite)))))

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
;; specification [1]. Special care is taken to produce proper values for
;; fields 2 and 4: according to Vixie specification "If both fields are
;; restricted (ie, aren't *), the command will be run when _either_ field
;; matches the current time." This implies that if one of these fields is *,
;; while the other is not, its value should be '() [0], otherwise
;; interpolate-weekdays below will produce incorrect results.

;; When we create the return procedure, it is this list to
;; which references to a time-spec-list will be bound. It will be used by the
;; returned procedure [3] to compute the next time a function should run. Any
;; 7's in the weekday component of the list (the last one) are folded into 0's
;; (both values represent sunday) [2]. Any 0's in the month-day component of the
;; list are removed (this allows a solitary zero to be used to indicate that
;; jobs should only run on certain days of the _week_) [2.1].
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
;;   The new time is then converted back into a UNIX time and returned [7].

(define (parse-vixie-time string)
  (let ((tokens (string-tokenize (vixie-substitute-parse-symbols string))))
    (cond
     ((> (length tokens) 5)
      (throw 'mcron-error 9
             "Too many fields in Vixie-style time specification"))
     ((< (length tokens) 5)
      (throw 'mcron-error 9
             "Not enough fields in Vixie-style time specification")))
    (let ((time-spec-list
           (map-in-order (lambda (x) (vector
                                      (let* ((n (vector-ref x 0))
                                             (tok (list-ref tokens n)))
                                        (cond
                                         ((and (= n 4)
                                               (string=? tok "*")
                                               (not (string=?
                                                     (list-ref tokens 2) "*")))
                                          '())
                                         ((and (= n 2)
                                               (string=? tok "*")
                                               (not (string=?
                                                     (list-ref tokens 4) "*")))
                                          '())
                                         (else
                                          (parse-vixie-element
                                           tok
                                           (vector-ref x 1)
                                           (vector-ref x 2)))))  ; [0]
                                      (vector-ref x 3)
                                      (vector-ref x 4)))
                 ;; token range-top+1   getter    setter
                 `( #( 0     0     60      ,tm:min   ,set-tm:min   )
                    #( 1     0     24      ,tm:hour  ,set-tm:hour  )
                    #( 2     1     32      ,tm:mday  ,set-tm:mday  )
                    #( 3     0     12      ,tm:mon   ,set-tm:mon   )
                    #( 4     0      7      ,tm:wday  ,set-tm:wday  )))))  ;; [1]

      (vector-set! (car (last-pair time-spec-list))
                   0
                   (map (lambda (time-spec)
                          (if (eqv? time-spec 7) 0 time-spec))
                        (vector-ref (car (last-pair time-spec-list)) 0))) ;; [2]

      (vector-set! (caddr time-spec-list)
                   0
                   (remove (lambda (day) (eqv? day 0))
                           (vector-ref (caddr time-spec-list) 0)))  ;; [2.1]


      (lambda (current-time)     ;; [3]
        (let ((time (localtime current-time)))  ;; [4]

          (if (not (member (tm:mon time)
                           (time-spec:list (cadddr time-spec-list))))
              (begin
                (nudge-month! time (cdddr time-spec-list))
                (set-tm:mday  time 0)))
          (if (or (eqv? (tm:mday time) 0)
                  (not (member (tm:mday time)
                               (interpolate-weekdays
                                (time-spec:list (caddr time-spec-list))
                                (time-spec:list (caddr (cddr time-spec-list)))
                                (tm:mon time)
                                (tm:year time)))))
              (begin
                (nudge-day! time (cddr time-spec-list))
                (set-tm:hour time -1)))
          (if (not (member (tm:hour time)
                           (time-spec:list (cadr time-spec-list))))
              (begin
                (nudge-hour! time (cdr time-spec-list))
                (set-tm:min time -1)))   ;; [5]

          (set-tm:sec time 0)
          (nudge-min! time time-spec-list)  ;; [6]
          (car (mktime time))))))) ;; [7]


