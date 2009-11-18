;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                  ;;;
;;; Confidential and proprietary information of ITA Software, Inc.   ;;;
;;;                                                                  ;;;
;;; Copyright (c) 2005-2008 ITA Software, Inc.  All rights reserved. ;;;
;;;                                                                  ;;;
;;; Original author: Matt Marjanovic                                 ;;;
;;;                                                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "QUUX")


;;; Date and time functions

;; These are specific aliases for representing time quantities as integers,
;; whether local or UTC.  The basic form is as a Lisp 'universal time',
;; i.e. seconds from the Lisp epoch.
;;
;; Although they are all interchangeable here in Lisp-world, these types
;; are stored differently (e.g. truncated) in the DB-world!
;; (See instances of 'define-db-class' for examples.)

;; 'time' means fully-specified moment (i.e. "date + tofd").
(deftype integer-time () 'integer)

;; 'date' means just year, month, day (tofd components zero or ignored).
(deftype integer-date () 'integer)

;; 'tofd' means just hour, minute, second (date components zero or ignored).
(deftype integer-tofd () 'integer)

;; Number of seconds.
(deftype integer-duration () 'integer)

(eval-when (:compile-toplevel :load-toplevel :execute)

(defconstant-equal +400-years-of-seconds+ 
  (- (cl:encode-universal-time 0 0 0 1 4 (+ 400 2010) 0)
     (cl:encode-universal-time 0 0 0 1 4 2010 0))
  "The number of seconds in four hundred years")

(defun encode-universal-time (second minute hour date month year &optional time-zone)
  "Extend to return negative values for years < 1900"
  (check-type year (integer 0))
  (if (< 99 year 1900)
    (- (encode-universal-time second minute hour date month (+ year 400) time-zone)
       +400-years-of-seconds+)
    (cl:encode-universal-time second minute hour date month year time-zone)))

(defun decode-universal-time (universal-time &optional time-zone)
  "Extend to decode negative numbers"
  (if (< universal-time 0)
    (multiple-value-bind (second minute hour date month year day daylight timezone)
        (decode-universal-time (+ universal-time +400-years-of-seconds+) time-zone)
      (values second minute hour date month (- year 400) day daylight timezone))
    (cl:decode-universal-time universal-time time-zone)))

(defconstant $1week #.(* 24 60 60 7)
  "The duration of one week, in integer-time.")
(defconstant $24hours #.(* 24 60 60)
  "The duration of one day, in integer-time.")
(defconstant $12hours #.(* 12 60 60)
  "The duration of one half-day, in integer-time.")
(defconstant $4hours #.(* 4 60 60)
  "The duration of four hours, in integer-time.")
(defconstant $1hour #.(* 1 60 60)
  "The duration of one hour, in integer-time.")
(defconstant $1minute #.(* 1 60)
  "The duration of one minute, in integer-time.")
(defconstant $1second 1
  "The duration of one second, in integer-time.")

(defconstant $largest-tz-offset #.(* 14 60 60)
  "The largest possible timezone offset, in integer-time.")

(defconstant $first-minute-of-day 0
  "The first minute of a day (i.e. 00:00, midnight), in integer-time.")

)	;eval-when


(eval-when (:compile-toplevel :load-toplevel :execute)

;; If you liked this constant, you might also want to try our related
;; function, last-minute-of-local-date (see below).
(defconstant $last-minute-of-day #.(- $24hours $1minute)
  "The last minute of a day (i.e. 23:59), in integer-time.")

(defconstant $time-in-distant-past 0
  "A time in the distant past.")

(defconstant $time-in-distant-future
  #.(encode-universal-time 0 0 0 1 1 2525 0)
  "A time in the distant future.")

)	;eval-when


(defun-inline local-tofd-only (time)
  "Zero the date bits of a integer-time, leaving the tofd bits alone."
  (check-types (integer-time time))
  (mod time $24hours))

(defun-inline local-date-only (time)
  "Zero the time bits of a integer-time, leaving the date bits alone."
  (check-types (integer-time time))
  (- time (local-tofd-only time)))


(defun-inline first-minute-of-local-date (time)
  "Return the integer-time representing the first minute on the given date."
  (local-date-only time))

(defun last-minute-of-local-date (time)
  "Return the integer-time representing the last minute on the given date."
  (check-types (integer-time time))
  (+ (local-date-only time) $last-minute-of-day))

(defun roll-time (time &key (seconds 0)
                            (minutes 0)
                            (hours 0)
                            (days 0) 
                            (months 0)
                            (years 0))
  "Return an integer-time before/after TIME by SECONDS, MINUTES,
HOURS, DAYS, MONTHS or YEARS.

TIME - an integer-time (number of seconds since epoch).

SECONDS, MINUTES, HOURS, DAYS, MONTHS, YEARS - integers.

ROLL-TIME operates by adding/subtracting SECONDS, MINUTES, HOURS,
DAYS, MONTHS and YEARS (in that order) to the facsimile time
TIME. ROLL-TIME treats TIME simply as the number of seconds since
epoch ignoring DST, time zones and anything else which isn't
passed as an argument to `encode-integer-time'.

Notes:

1) The length of months, expressed as a number of days,
varies. Therefore it does not always hold that:

  (= x (roll-time (roll-time x :months M :years Y)
       :months -M :years -Y))

2) If the result of rolling, even in an interedmiate step, ends
   up before 1900-01-01T00:00:00 we lose. Example:

 (roll-time (encode-universal-time 0 0 0 1 1 1900)
            :days -31 :months 3)
 ==> ERROR"
  (check-type time integer-time)
  (check-type seconds integer)
  (check-type minutes integer)
  (check-type hours integer)
  (check-type days integer)
  (check-type months integer)
  (check-type years integer)
  ;; seconds
  (setf time (+ time seconds))
  ;; minutes
  (setf time (+ time (* $1minute minutes)))
  ;; hours
  (setf time (+ time (* $1hour hours)))
  ;; days
  (setf time (+ time (* $24hours days)))
  ;; months & years
  (multiple-value-bind (sec min hour day start-month start-year)
      (decode-integer-time time)
    (let* ((month-delta (mod months 12))
           (year-delta (+ years (floor months 12)))
           (new-month (+ start-month month-delta))
           (new-year (+ start-year year-delta)))
      (cond
        ((> new-month 12)
         (setf new-month (- new-month 12)
               new-year (+ new-year 1))))
      (setf time (encode-integer-time sec min hour
                                      (min day (days-per-month new-month new-year))
                                      new-month new-year))))
  time)



;;---!!!mm  Write unit-tests and documentation for this.
(defun advance-local-time (time &key (seconds 0) (minutes 0) (hours 0) (days 0))
  (check-types (integer-time time)
               (integer seconds minutes hours days))
  (+ time
     (* $1second seconds)
     (* $1minute minutes)
     (* $1hour hours)
     (* $24hours days)))


;;; Zoned times

;; A zoned-time has readers, not accessors, on purpose -- it needs to
;;  be an atomic, immutable object.  Otherwise, DB and other consistency
;;  becomes a maintenance/engineering nightmare.
;;
;; Note that variables which contain a zoned-time will have the suffix "-zul"
;;  (which is the short acronym for "zone, utc, local").
(defclass zoned-time ()
  ((time-utc :type integer-time
	     :reader utc-time
	     :initarg :utc
	     :documentation "UTC integer-time representation of moment")
   (tz-offset :type integer-time
	      :reader tzoffset
	      :initarg :tzoffset
	      :documentation "Offset (seconds) to local timezone (from UTC)"))
  (:documentation
   "An immutable time object which includes timezone information.
   If both local and UTC views are ever relevant for some absolute temporal
   quantity, then a zoned-time should be used to represent that quantity."))


(defmethod print-object ((zul zoned-time) stream)
  "Pretty-print a zoned-time object."
  (print-unprintable-object (zul stream :type t :identity t)
    (write-zoned-time zul stream
		      :date-as :yyyy-mm-dd :time-as :hh-mm-ss
		      :show-timezone t)))

(defun make-zoned-time (&key utc local tzoffset)
  (assert (and (not (and utc local tzoffset))
               (if tzoffset
                 (or utc local)
                 (and utc local)))
          () "Exactly two of time, local, tzoffset must be specified.")
  (make-instance 'zoned-time
    :utc (or utc
             (- local tzoffset))
    :tzoffset (or tzoffset
                  (- local utc))))

(defgeneric zoned-time (time-designator)
  (:documentation
   "Transform the argument into a ZONED-TIME instance.
 A string is parse  in ISO8601 syntax, a ZONED-TIME instance is returned, and an integer is used as
 the utc argument to construct a new ZONED-TIME instance with offset 0."))

(defmethod zoned-time ((time-designator string))
  (parse-iso8601-zoned time-designator))

(defmethod zoned-time ((zoned-time zoned-time))
  zoned-time)

(defmethod zoned-time ((utc-time integer))
  (make-zoned-time :utc utc-time :tzoffset 0))



(defmethod local-time ((zul zoned-time))
  "Return the local time of a zoned-time object.

  Returns an 'integer-time', i.e. seconds since the beginning of the
  lisp epoch."
  (+ (utc-time zul) (tzoffset zul)))

(defun get-local-time (time)
  (etypecase time
    (integer-time time)
    (zoned-time (local-time time))))

(defmethod duration ((a zoned-time) (b zoned-time))
  "Compute the signed duration (in seconds) from moment 'a' to moment 'b'.

  'a' and 'b' are 'zoned-time' quantities.
   If b is later than a, then the result is positive."
  (- (utc-time b) (utc-time a)))

(defmethod zoned-time-equal-p ((a zoned-time) (b zoned-time))
  "Return T if zoned-times A and B are equal (same time and tzoffset)"
  (and (eql (utc-time a) (utc-time b))
       (eql (tzoffset a) (tzoffset b))))

(defun utc-is-before-utc-p (a b)
  "Return T if 'a' precedes 'b' in absolute (UTC) time."
  (check-type a zoned-time)
  (check-type b zoned-time)
  (< (utc-time a) (utc-time b)))

(defun utc-is-after-utc-p (a b)
  "Return T if 'a' succeeds 'b' in absolute (UTC) time."
  (check-type a zoned-time)
  (check-type b zoned-time)
  (> (utc-time a) (utc-time b)))


(defmethod make-zoned-date ((z zoned-time))
  "Create a zoned-time matching input, but with zero'ed time components."
  (make-instance 'zoned-time
    :tzoffset (tzoffset z)
    :utc (local-date-only (utc-time z))))


;;---!!!mm  WHO ADDED THIS??  IT IS BROKEN.                             
(defmethod add-days ((z zoned-time) days)
  "Add some number of days to a zoned-time."
  (if (zerop days)
    z
    (make-instance 'zoned-time
      :tzoffset (tzoffset z)
      :utc (+ (* days $24hours) (utc-time z)))))


;;; Time caching & rigging

(defun-inline server-system-time-utc ()
  "Return the server's true UTC system time (as an integer-time).

  This function returns the real UTC time as seen by the server's clock,
  and should only be used in situations such as logging, where the true
  physical clock time is important.

  Otherwise, one should always use CURRENT-TIME-UTC."
  (get-universal-time))

(defvar *current-time-function* #'get-universal-time)

(defun current-time-utc ()
  "Return the 'current' UTC time (as an integer-time).

  The 'current' time may have been cached by 'with-current-time-cached',
  or it may be skewed by 'with-current-time-function'."
  (funcall *current-time-function*))

(let ((last-overridden-time-utc nil)
      (last-overridden-time-utc-string nil))

  (defun current-time-utc-db-override ()
    (if (eq *current-time-function* #'get-universal-time)
      nil
      (let ((now (current-time-utc)))
	(cond ((eql now last-overridden-time-utc)
	       last-overridden-time-utc-string)
	      (t
	       (setq last-overridden-time-utc now
		     last-overridden-time-utc-string
		     (with-output-to-string (str)
		       (write-iso8601-utc now str)))))))))

(defun current-time-zoned ()
  "Return the 'current' UTC time (as a zoned time).

  The 'current' time may have been cached by 'with-current-time-cached',
  or it may be skewed by 'with-current-time-function'."
  (make-instance 'zoned-time :utc (current-time-utc) :tzoffset 0))

(defmacro with-current-time-cached ((&optional override-time-utc) &body body)
  "Create a scope which caches/freezes the current UTC time.

  This scope caches the result of (current-time-utc).  Evaluations of
  (current-time-utc) within the scope will return the cached value.
  If the optional 'override-time-utc' is provided, then the scope will
  cache that value instead."
  `(let* ((now (or ,override-time-utc
                   (current-time-utc)))
          (*current-time-function* (lambda () now)))
    ,@body))

(defmacro without-current-time-trickery (&body body)
  "Create a scope that undoes any of the current-time caches/functions."
  `(let ((*current-time-function* #'get-universal-time))
    ,@body))

(defmacro without-current-time-cached (&body body)
  "DEPRECATED; use 'without-current-time-trickery' instead."
  `(without-current-time-trickery
   ,@body))


(defmacro with-current-time-function ((new-time-function
                                       &key allow-nesting?) &body body)
  "Create a scope in which the current time is generated by a function.

  Within this scope, evaluations of (current-time-utc) will report the
  time as generated by 'new-time-function', instead of the time as
  reported by the operating system.

  For example, to create a scope where time runs 5x faster than normal:
    (with-current-time-function ((time-speeder-upper 5))
      (process-sleep 1)
      (current-time-utc))   ; This returns a time 4s in the future

  These scopes may be nested. Nesting is disabled by default, since you're
  probably shooting yourself in the foot by it.

  The inner-most function is first executed; if it refers to
  (current-time-utc), which (time-speeder-upper) does, then the function in the
  outer scope will provide that value. E.g. nesting two (tims-speeder-upper)s
  will have the effect of speeding time by the product of their rates."
  (with-gensyms (original-time-function-v new-time-function-v)
    `(let* ((,original-time-function-v *current-time-function*)
            (,new-time-function-v ,new-time-function)
            (*current-time-function*
             (lambda ()
               (let ((*current-time-function* ,original-time-function-v))
                 (funcall ,new-time-function-v)))))
      ,@(unless allow-nesting?
                `((unless (eq ,original-time-function-v #'get-universal-time)
                    (error "Nested 'with-current-time-function'."))))
      ,@body)))

(defmacro with-current-integer-time-fixed ((integer-time) &body body)
  `(let ((*current-time-function* (constantly ,integer-time)))
     ,@body))

(defmacro with-current-time-fixed ((second minute hour day month year) &body body)
  `(with-current-integer-time-fixed ((encode-integer-time ,second ,minute ,hour ,day ,month ,year))
     ,@body))

(defmacro with-virtual-sleep ((&key (days 0) (hours 0) (mins 0) (secs 0)) &body body)
  `(let* ((previous-time-function *current-time-function*)
          (*current-time-function* (lambda ()
                                     (roll-time (funcall previous-time-function)
                                                :days    ,days
                                                :hours   ,hours
                                                :minutes ,mins
                                                :seconds ,secs))))
     ,@body))

(defun time-speeder-upper (rate)
  "Returns a lambda that generates time at 'rate' x normal rate.

  For use in the (with-current-time-function) macro.

  Note that this clock still 'ticks' once per wallclock second; it is just that each tick moves
  the time forward a large amount. If 'current-time-utc' is called multiple times within that
  second, time will appear to be frozen.

  See 'time-speeder-upper-hires' for a clock that ticks more often."
  (let ((now (current-time-utc)))
    (lambda ()
      (+ now (* rate
                (- (current-time-utc) now))))))

(defun time-speeder-upper-hires (rate)
  "Returns a lambda that generates time at 'rate' x normal rate, at high resolution.

  For use in the (with-current-time-function) macro.

  This clock ticks at the rate given by 'internal-time-units-per-second', typically 100.

  Note: this clock breaks nesting, since it only relies on the outer scope to set
  the initial time for the clock."
  (let ((now (current-time-utc))
        (base-realtime (get-internal-real-time)))
    (lambda ()
      (truncate
       (+ now (* (/ rate internal-time-units-per-second)
                 (- (get-internal-real-time) base-realtime)))))))


;;; Date and time manipulation

(defun-inline round-time-to-next-minute (time)
  "Round up an integer-time to the nearest larger minute.

   Returns an integer number of minutes, which, when multiplied by $1minute, will be 0-60 greater
   than the input."
  (check-types (integer-time time))
  (ceiling time $1minute))


;; Lisp's encode-universal-time and decode-universal-time functions
;; are swell, but slow.  (They also have this annoying "tz" argument,
;; which should *always* be zero in QRS's usage.)  The issue of bignum
;; arithmetic is unavoidable if the timebase is in seconds, until we
;; have a 64-bit lisp.  However, we can speed up these functions by
;; a factor of 2-3 by precomputing most of the results over a range
;; of dates.  Hence, the encode- and decode- functions defined below.
;;
;; The date range is [1900,2100] (all months/days inclusive),
;; set immediately below.

(eval-when (:compile-toplevel :load-toplevel :execute)

(defconstant $it-cache-first-year 1900
  "The first year of precomputed integer-time en-/de-coding")
(defconstant $it-cache-last-year  2100
  "The last year of precomputed integer-time en-/de-coding")

)	;eval-when


(eval-when (:compile-toplevel :load-toplevel :execute)

(defconstant $it-cache-first-day
  (floor (encode-universal-time 0 0 0 1 1 $it-cache-first-year 0)
         $24hours)
  "The first day (since epoch) of precomputed integer-time en-/de-coding.")

(defconstant $it-cache-last-day
  (floor (encode-universal-time 0 0 0 31 12 $it-cache-last-year 0)
         $24hours)
  "The last day (since epoch) of precomputed integer-time en-/de-coding.")

)	;eval-when


(eval-when (:compile-toplevel :load-toplevel :execute)

(defconstant-equalp $days-until-year-month
  (let ((v (make-array (i* 13 (i+ 1 (i- $it-cache-last-year
                                        $it-cache-first-year))))))
    (do ((year $it-cache-first-year (1+ year)))
        ((> year $it-cache-last-year) v)
      (declare (type fixnum year))
      (do ((month 0 (1+ month)))
          ((> month 12))
        (declare (type fixnum month))
        (setf (svref v (i+ month (i* 13 (i- year $it-cache-first-year))))
              (let* ((month (if (i= month 0) 1 month))
                     (time (encode-universal-time 0 0 0 1 month year 0)))
                (floor time $24hours))))))
  "An array which maps (year,month) --> (days preceding that year/month)
   where 'days' starts at the lisp epoch.
   The map starts with the first month of year '$it-cache-first-year'
   and ends with the last month of year '$it-cache-last-year'.
   And, there are 13 months in a year -- i.e. [0,12] are valid months,
   and month 0 is the same as month 1.")

(defun-inline %days-until-year-month (year month)
  "Return the number of days since the lisp epoch which precede the
   given (year, month).  For example:  (1900 2) --> 31 days."
  (svref $days-until-year-month
         (i+ month (i* 13 (i- year $it-cache-first-year)))))

)	;eval-when


(eval-when (:compile-toplevel :load-toplevel :execute)

(defconstant-equalp $days-per-february-year
  (let ((v (make-array (i+ 1 (i- $it-cache-last-year
                                 $it-cache-first-year)))))
    (do ((year $it-cache-first-year (1+ year)))
        ((> year $it-cache-last-year) v)
      (declare (type fixnum year))
      (setf (svref v (i- year $it-cache-first-year))
            (i- (%days-until-year-month year 3)
                (%days-until-year-month year 2)))))
  "An array which maps (year) --> (days in february that year)
   The map starts with the first month of year '$it-cache-first-year'
   and ends with the last month of year '$it-cache-last-year'.")

(defun-inline %days-per-february-year (year)
  "Return the number of days in February of the given year
   For example:  1900 --> 28 days."
  (svref $days-per-february-year
         (i- year $it-cache-first-year)))

)	;eval-when


(defconstant-equalp $days-per-month
  (make-array 12 :initial-contents '(31 28 31 30 31 30 31 31 30 31 30 31))
  "An array mapping (month) --> (days in month)
   The value for February is, of course, incorrect during leap years,
   hence the need for %days-per-february-year defined above.")


;; The following few definitions are defparameter instead of a
;; defconstant because the latter is always (in SBCL and CMCL)
;; evaluated at compile time.  We don't need these at compile time,
;; and they slow down the build by around 30 seconds for no good
;; reason. -- DLW 10/6/06

;; See note above for why this is a defparameter instead of a defconstant.
(defparameter $days-until-month
  (let ((v (make-array 12 :initial-element 0)))
    (loop for m from 0 below 12
          for total = 0 then (+ total next)
          for next = (svref $days-per-month m)
          doing (setf (svref v m) total))
    v)
  "An array mapping (month) --> (days before beginning of month)
   The value for months after February is, of course, incorrect
   during leap years.")


;; See note above for why this is a defparameter instead of a defconstant.
(defparameter $year-from-days
  (let ((v (make-array (- $it-cache-last-day $it-cache-first-day -1))))
    (do ((days $it-cache-first-day (1+ days)))
        ((> days $it-cache-last-day) v)
      (multiple-value-bind (s m h day month year)
          (decode-universal-time (* days $24hours) 0)
        (declare (ignore s m h day month))
        (setf (svref v days) year))))
  "An array which maps (days since epoch) --> (year)
   The zeroth entry in the array is for $it-cache-first-day,
   the last entry is for $it-cache-last-day.")

;; See note above for why this is a defparameter instead of a defconstant.
(defparameter $month-from-days
  (let ((v (make-array (- $it-cache-last-day $it-cache-first-day -1))))
    (do ((days $it-cache-first-day (1+ days)))
        ((> days $it-cache-last-day) v)
      (multiple-value-bind (s m h day month year)
          (decode-universal-time (* days $24hours) 0)
        (declare (ignore s m h day year))
        (setf (svref v days) month))))
  "An array which maps (days since epoch) --> (month-of-year),
   where month-of-year is in the range [1,12].
   The zeroth entry in the array is for $it-cache-first-day,
   the last entry is for $it-cache-last-day.")

;; See note above for why this is a defparameter instead of a defconstant.
(defparameter $day-from-days
  (let ((v (make-array (- $it-cache-last-day $it-cache-first-day -1))))
    (do ((days $it-cache-first-day (1+ days)))
        ((> days $it-cache-last-day) v)
      (multiple-value-bind (s m h day month year)
          (decode-universal-time (* days $24hours) 0)
        (declare (ignore s m h month year))
        (setf (svref v days) day))))
  "An array which maps (days since epoch) --> (day-of-month)
   where day-of-month is in the range [1,N].
   The zeroth entry in the array is for $it-cache-first-day,
   the last entry is for $it-cache-last-day.")

;; See note above for why this is a defparameter instead of a defconstant.
(defparameter $dofw-from-days
  (let ((v (make-array (- $it-cache-last-day $it-cache-first-day -1))))
    (do ((days $it-cache-first-day (1+ days)))
        ((> days $it-cache-last-day) v)
      (multiple-value-bind (s m h day month year weekday)
          (decode-universal-time (* days $24hours) 0)
        (declare (ignore s m h day month year))
        (setf (svref v days) weekday))))
  "An array which maps (days since epoch) --> (day-of-week)
   where day-of-week is in the range [0,6] for [mon,sun].
   The zeroth entry in the array is for $it-cache-first-day,
   the last entry is for $it-cache-last-day.")


(defun-inline %year-from-2digit-year (2digit-year)
  "Convert an ambiguous 2-digit year into an unambiguous 4+-digit year.

  Years before '70 are assumed to be in the 2000's.
  '70 and later are assumed to be in the 1900's.
  This function doesn't check that input *is* a 2-digit year."
  (if (i>= 2digit-year 70)
    (i+ 2digit-year 1900)
    (i+ 2digit-year 2000)))

(defun days-per-month (month year)
  "Return the number of days in a given month in a given year.

  'month' is in the range [1,12] (where 1 == January).
  'year' is the year -- which only affects the result for February."
  (check-types (fixnum month year))
  (if (i= month 2)
    (if (and (i>= year $it-cache-first-year)
	     (i<= year $it-cache-last-year))
      (%days-per-february-year year)
      (let ((feb-first (encode-universal-time 0 0 0 1 2 year 0))
	    (mar-first (encode-universal-time 0 0 0 1 3 year 0)))
	(floor (- mar-first feb-first) $24hours)))
    (svref $days-per-month (i- month 1))))

(defun encode-integer-time (seconds minutes hours day month year)
  "Encode a broken-down time specification into integer-time.
  Input is the seconds, minutes, hours, day, month, and year.
  Result is an integer-time (seconds since lisp epoch).

  This function is equivalent to (encode-univeral-time ... 0),
  but is precomputed and thus faster for years in the range
  [$it-cache-first-year,$it-cache-last-year]."
  (check-types (fixnum seconds minutes hours day month year))
  (when (i< year 100)
    (setq year (%year-from-2digit-year year)))
  (if (and (i>= year $it-cache-first-year)
           (i<= year $it-cache-last-year))
    (let ((days (i+ (%days-until-year-month year month)
		    (i- day 1)))
	  (seconds (i+ seconds
		       (i+ (i* minutes $1minute)
			   (i* hours $1hour)))))
      (+ (* days $24hours) seconds))
    (encode-universal-time seconds minutes hours day month year 0)))


(defun encode-integer-date (day month year)
  "Encode a broken-down date specification into integer-time.
  Input is the day, month, and year.
  Result is an integer-time (seconds since lisp epoch).

  This function is equivalent to (encode-integer-time 0 0 0 ...),
  but can be 3x as fast."
  (check-types (fixnum day month year))
  (when (i< year 100)
    (setq year (%year-from-2digit-year year)))
  (if (and (i>= year $it-cache-first-year)
           (i<= year $it-cache-last-year))
    (* (i+ (%days-until-year-month year month) (i- day 1)) $24hours)
    (encode-universal-time 0 0 0 day month year 0)))

;; check to make sure it's not time to revisit the function "%year-from-2digit-year" above
(assert (utc-is-before-utc-p
         (make-instance 'zoned-time :utc (server-system-time-utc))
         (make-instance 'zoned-time :utc (encode-integer-date 01 01 2030))))


(defun encode-integer-tofd (seconds minutes hours)
  "Encode a broken-down time-of-day specification into integer-time.
  Input is the seconds, minutes, and hours.
  Result is an integer-tofd (seconds since midnight).

  This function is equivalent to (encode-integer-time ... 0 0 0),
  but is 7.5x as fast and does almost no consing (since a tofd always
  fits in a fixnum."
  (check-types (fixnum seconds minutes hours))
  (i+ seconds
      (i+ (i* minutes $1minute)
          (i* hours $1hour))))


(defun decode-integer-time (time)
  "Decode an integer-time into its broken-down components.

  Input is an integer-time.
  Result is the values
      <seconds> <minutes> <hours> <day> <month> <year> <dofw>.
  This function is equivalent to (decode-universal-time ... 0),
  but is precomputed and thus faster for years in the range
  [$it-cache-first-year,$it-cache-last-year]."
  (check-types (integer-time time))
  (multiple-value-bind (days tofd) (floor time $24hours)
    (declare (type fixnum days tofd))
    (if (and (i>= days $it-cache-first-day)
             (i<= days $it-cache-last-day))
      (multiple-value-bind (hours tofd-min) (floor tofd $1hour)
	(declare (type fixnum hours tofd-min))
	(multiple-value-bind (minutes seconds) (floor tofd-min $1minute)
	  (declare (type fixnum minutes seconds))
	  (values seconds minutes hours
		  (svref $day-from-days days)
		  (svref $month-from-days days)
		  (svref $year-from-days days)
		  (svref $dofw-from-days days))))
      (multiple-value-bind (seconds minutes hours day month year weekday)
	  (decode-universal-time time 0)
	(values seconds minutes hours day month year weekday)))))


(defun decode-integer-date (date)
  "Decode an integer-time into only its broken-down date components.

  Input is an integer-time.
  Result is the values <day> <month> <year> <dofw>.
  This function is equivalent to (decode-universal-time ... 0),
  but is precomputed and thus faster for years in the range
  [$it-cache-first-year,$it-cache-last-year]."
  (check-types (integer-date date))
  (let ((days (floor date $24hours)))
    (declare (type fixnum days))
    (if (and (i>= days $it-cache-first-day)
             (i<= days $it-cache-last-day))
      (values (svref $day-from-days days)
	      (svref $month-from-days days)
	      (svref $year-from-days days)
	      (svref $dofw-from-days days))
      (multiple-value-bind (seconds minutes hours day month year weekday)
	  (decode-universal-time date 0)
	(declare (ignore seconds minutes hours))
	(values day month year weekday)))))


(defun decode-integer-tofd (tofd)
  "Decode an integer-time into only its broken-down time-of-day components.

  Input is an integer-time.
  Result is the values <seconds> <minutes> <hours>.
  This function is equivalent to (decode-universal-time ... 0),
  but is precomputed and thus faster for years in the range
  [$it-cache-first-year,$it-cache-last-year]."
  (check-types (integer-tofd tofd))
  (let ((tofd (mod tofd $24hours)))
    (declare (type fixnum tofd))
    (multiple-value-bind (hours tofd-min) (floor tofd $1hour)
      (declare (type fixnum hours tofd-min))
      (multiple-value-bind (minutes seconds) (floor tofd-min $1minute)
        (declare (type fixnum minutes seconds))
        (values seconds minutes hours)))))


(defun-inline %days-until-month (month year)
  "Return the number of days preceding the given month in the given year.
   For example:  March, 2004 --> (3 2004) --> 60 days"
  (i+ (svref $days-until-month (i1- month))
      (if (and (i= 29 (%days-per-february-year year))
               (> month 2))
          1
          0)))


(defun day-of-year (time)
  "Extract the 1-based day-of-year from an integer-time.
    E.g. January 1 (of any year) --> 1

  'time' is an integer-time.
  Result is an integer in the range [1,366]."
  (check-types (integer-time time))
  (let ((days (floor time $24hours)))
    (declare (type fixnum days))
    (if (and (i>= days $it-cache-first-day)
             (i<= days $it-cache-last-day))
      ;; Using cached time constants
      (let ((day   (svref $day-from-days days))
            (month (svref $month-from-days days))
            (year  (svref $year-from-days days)))
        ;; (Days from epoch til 'time') - (Days from epoch til January)
        (i+ day (%days-until-month month year)))
      ;; Outside of time cache
      (multiple-value-bind (s m h day month year)
          (decode-universal-time time 0)
        (declare (ignore s m h))
        (i+ day
            (loop for m from 1 below month
                  sum (days-per-month m year)))))))


(defun local-date-offset (time1 time2)
  "Return the (integer) difference in _date_ between two local times.

  This is essentially the difference in 'printed days', not the
  difference in '24-hour periods'.
  For example, '2005-03-05 23:00' and '2005-03-06 01:00' will yield
  a 1 day difference in date, even though they are only 2 hours apart.

  'time1' and 'time2' are local 'integer-times'.
  The result is positive if time2 is later than time1."
  (check-types (integer-time time1 time2))
  (- (floor time2 $24hours) (floor time1 $24hours)))


(defun-inline local-time-to-utc (time-local tzoffset)
  "Given a local integer-time and a TZ offset, return the UTC integer-time."
  (- time-local tzoffset))

(defun-inline utc-time-to-local (time-utc tzoffset)
  "Given a UTC integer-time and a TZ offset, return the local integer-time."
  (+ time-utc tzoffset))


(defun local-date-equal (date1 date2)
  "Return T if 'date1' and 'date2' have equal day, month, and year.

  'date1' and 'date2' are integer-time's."
  (= (local-date-only date1) (local-date-only date2)))


(defun merge-date-and-tofd (date tofd)
  "Merge a 'date' with a 'tofd'.

   Take the date components of 'date' (an integer-time)
   and merge them with the time components of 'tofd' (another
   integer-time), generating a new integer-time.

   ('date' and 'time' should be local to the same TZ, although
   the result is effectively in the same relative TZ as 'tofd'.)"
  (check-types (integer date tofd))
  (+ (local-date-only date) (local-tofd-only tofd)))


(defun local-date-to-utc (local-date local-tofd tzoffset)
  "Convert the given LOCAL-DATE/LOCAL-TOFD to UTC and return the date part.
   For example: 1-MAR 23:00 EST -> 2-MAR"
  (local-date-only (local-time-to-utc (merge-date-and-tofd local-date local-tofd) tzoffset)))

(defun utc-date-to-local (utc-date local-tofd tzoffset)
  "Return the local date for the given local time and UTC date.
   For example: 2-MAR 23:00 EST -> 1-MAR"
  (local-date-only (utc-time-to-local (merge-date-and-tofd utc-date (local-time-to-utc local-tofd tzoffset)) tzoffset)))

(defun override-local-time (time &key seconds minutes hours day month year)
  "Override specific date/tofd components of an integer-time.

  Returns the new integer-time constructed by replacing the specified
  components of the original integer-time 'time'."
  (check-types (integer-time time)
	       ((or integer null) seconds minutes hours day month year))
  (multiple-value-bind (_seconds _minutes _hours _day _month _year)
      (decode-integer-time time)
    (encode-integer-time (or seconds _seconds)
                         (or minutes _minutes)
                         (or hours   _hours)
                         (or day     _day)
                         (or month   _month)
                         (or year    _year))))

;;!!!TZ
(defun advance-to-next-day-of-week (time
				    weekday ; 0-6 corresponds to mon-sun
				    &key keep-tofd-p)
  "Return the time corresponding to the next WEEKDAY calculated from TIME.
   WEEKDAY is an integer with 0 corresponding to Monday, 6 to Sunday.
   KEEP-TOFD-P, if non-NIL, indicates that the time portion of TIME should be
   carried over to the generated time.  By default, 0:00h is returned for the
   time potions.  Returns a timezone agnostic universal time."
  (check-types (integer-time time)
	       (fixnum weekday))
  (multiple-value-bind (seconds minutes hours day month year old-weekday)
      (decode-universal-time time 0)
    (+ (encode-universal-time (if keep-tofd-p seconds 0)
                               (if keep-tofd-p minutes 0)
                               (if keep-tofd-p hours 0)
                               day month year
                               0)
       (* $24hours (let ((v (- weekday old-weekday)))
                     (if (< v 1)
                       (+ v 7)
                       v))))))

;;; Time and date parsing

(defconstant-equalp $uppercase-months
  '#("JAN" "FEB" "MAR" "APR" "MAY" "JUN" "JUL" "AUG" "SEP" "OCT" "NOV" "DEC"))

(defconstant-equalp $mixedcase-months
  '#("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

(defconstant-equalp $lowercase-months
  '#("jan" "feb" "mar" "apr" "may" "jun" "jul" "aug" "sep" "oct" "nov" "dec"))

(defconstant-equalp $uppercase-months-long
  #("JANUARY" "FEBRUARY" "MARCH" "APRIL" "MAY" "JUNE"
    "JULY" "AUGUST" "SEPTEMBER" "OCTOBER" "NOVEMBER" "DECEMBER"))

(defconstant-equalp $mixedcase-months-long
  #("January" "February" "March" "April" "May" "June"
    "July" "August" "September" "October" "November" "December"))

(defconstant-equalp $lowercase-months-long
  #("january" "february" "march" "april" "may" "june"
    "july" "august" "september" "october" "november" "december"))


(defconstant-equalp $uppercase-days
  '#("MON" "TUE" "WED" "THU" "FRI" "SAT" "SUN"))

(defconstant-equalp $mixedcase-days
  '#("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))

(defconstant-equalp $lowercase-days
  '#("mon" "tue" "wed" "thu" "fri" "sat" "sun"))

(defconstant-equalp $uppercase-days-ws
  '#("MON " "TUE " "WED " "THU " "FRI " "SAT " "SUN "))

(defconstant-equalp $mixedcase-days-ws
  '#("Mon " "Tue " "Wed " "Thu " "Fri " "Sat " "Sun "))

(defconstant-equalp $lowercase-days-ws
  '#("mon " "tue " "wed " "thu " "fri " "sat " "sun "))

(defconstant-equalp $uppercase-days-long
  '#("MONDAY" "TUESDAY" "WEDNESDAY" "THURSDAY" "FRIDAY" "SATURDAY" "SUNDAY"))

(defconstant-equalp $mixedcase-days-long
  '#("Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday"))

(defconstant-equalp $lowercase-days-long
  '#("monday" "tuesday" "wednesday" "thursday" "friday" "saturday" "sunday"))


(defun month-name (month &key (size :short) (case :upper))
  "Return a string containing the English name of a numeric month.

  'month' belongs to the range [1,12] (1==January).
  'size' is one of :short (default) or :long, specifying size of name.
  'case' is one of :upper (default), :mixed or :lower.
  Return value is a string, or NIL if the month is not in [1,12]."
  (check-types (fixnum month))
  (when (and (>= month 1) (<= month 12))
    (svref (ecase size
             (:short (ecase case
                       (:upper $uppercase-months)
                       (:mixed $mixedcase-months)
                       (:lower $lowercase-months)))
             (:long (ecase case
                      (:upper $uppercase-months-long)
                      (:mixed $mixedcase-months-long)
                      (:lower $lowercase-months-long))))
           (i1- month))))

(defun dofw-name (dofw &key (size :short) (case :upper))
  "Return a string containing the English name of a numeric day-of-week.

  'dofw' belongs to the range [0,6] (0==Monday).
  'size' is one of :short (default) or :long, specifying size of name.
  'case' is one of :upper (default), :mixed or :lower.
  Return value is a string, or NIL if the dofw is not in [0,6]."
  (check-types (fixnum dofw))
  (when (and (>= dofw 0) (<= dofw 6))
    (svref (ecase size
             (:short (ecase case
                       (:upper $uppercase-days)
                       (:mixed $mixedcase-days)
                       (:lower $lowercase-days)))
             (:long (ecase case
                      (:upper $uppercase-days-long)
                      (:mixed $mixedcase-days-long)
                      (:lower $lowercase-days-long))))
           dofw)))

(defun local-date-long-dofw-name (time &key (case :upper))
  "Return a string with the full English name of the day-of-week."
  (check-types (integer-time time))
  (multiple-value-bind (day month year dofw)
      (decode-integer-date time)
    (declare (ignore day month year))
    (svref (ecase case
             (:upper $uppercase-days-long)
             (:lower $lowercase-days-long)
             (:mixed $mixedcase-days-long))
           dofw)))


(defun explode-local-tofd (string &key (start 0) (end (length string)))
  "Parse a string (or substring) as a local time-of-day.
  Valid formats are:    HHMM      HHMMSS
                     H..H:MM  H..H:MM:SS
  Returns multiple-values:  <seconds> <minutes> <hours>"
  (check-types (string string)
	       (integer start end))
  (multiple-value-let*
      (((end additional-hours twelve-is-zero)
        (cond ((ends-with string "AM" :end end)
               (values (- end 2) 0 t))
              ((ends-with string "PM" :end end)
               (values (- end 2) 12 t))
              (t (values end 0 nil))))
       (len (i- end start))
       (all-digits t)
       c1 c2
       hours minutes seconds)
    ;; find colons, check for digits
    (loop for i fixnum from start below end
          for c = (char string i)
          doing
          (unless (ascii-digit-p c) (setq all-digits nil))
          (when (eql c #\:)
            (cond ((not c1) (setq c1 i))
                  ((not c2) (setq c2 i))
                  (t (return-from explode-local-tofd (values nil nil nil))))))
    (if (and all-digits
             (or (i= len 4) (i= len 6)))
      ;; HHMM or HHMMSS
      (setq hours   (string-to-integer string
                                       :start start :end (i+ start 2))
            minutes (string-to-integer string
                                       :start (i+ start 2) :end (i+ start 4))
            seconds (and (i= len 6)
                         (string-to-integer string
                                            :start (i+ start 4) :end end)))
      ;; H..H:MM or H..H:MM:SS
      (setq hours   (and c1
                         (string-to-integer string :start start :end c1))
            minutes (and c1
                         (string-to-integer string
                                            :start (i1+ c1) :end (or c2 end)))
            seconds (and c2
                         (string-to-integer string :start (i1+ c2) :end end))))
    (let ((hours (if (and hours twelve-is-zero (= hours 12))
                   0
                   hours)))
      ;; S requires H and M, H and M require each other
      (values (and minutes hours seconds)
              (and hours minutes)
              (and minutes hours (i+ hours additional-hours))))))


(defun explode-local-time (string &key (start 0) (end (length string))
			   default-year)
  "Explode a string into (local) date and tofd components.
   Valid input formats are (maybe?):

   [D]D-MTH[-YY[YY]]
   [D]D-MM-YYYY
   [D]D[-]M[M][-]YYYY
   YYYY-MM-D[D]
   YYYY[-]M[M][-]D[D]
   DDMMYY

   MM-YY  - day is set to last day of month
   MM-YYYY  - day is set to last day of month




   Return values are:  <seconds> <minutes> <hours> <day> <month> <year>.
   'year' may be NIL if no year was supplied."
  (check-types (string string)
	       (fixnum start end))
  (let ((point start)
	(seconds  0)
	(minutes  0)
	(hours 0)
	(day   nil)
	(month nil)
	(year  nil))
    (labels ((punct (&optional (optional-p t))
	       (or (when (i<= point (i- end 1))
		     (let ((ch (char string point)))
		       (when (or (eql ch #\.) (eql ch #\-) (eql ch #\/))
			 (iincf point)
			 t)))
		   optional-p))
	     (parse-day1 ()
	       (when (i<= point (1- end))
		 (let ((d (ascii-digit-p (char string point))))
		   (when (and d (i> d 0))
		     (iincf point)
		     (setq day d)
		     t))))
	     (parse-day2 ()
	       (when (i<= point (i- end 2))
		 (let ((d1 (ascii-digit-p (char string point)))
		       (d2 (ascii-digit-p (char string (i+ point 1)))))
		   (when (and d1 d2)
		     (let ((d12 (i+ (i* d1 10) d2)))
		       (when (or (i= d12 0)
				 (i= d12 99)
				 (and (i<= d12 31) (i> d12 0)))
			 (iincf point 2)
			 (setq day d12)
			 t))))))
	     (parse-month1 ()
	       (when (i<= point (1- end))
		 (let ((d (ascii-digit-p (char string point))))
		   (when (and d (i> d 0))
		     (iincf point 1)
		     (setq month d)
		     t))))
	     (parse-month2 ()
	       (when (i<= point (i- end 2))
		 (let ((d1 (ascii-digit-p (char string point)))
		       (d2 (ascii-digit-p (char string (i+ point 1)))))
		   (when (and d1 d2)
		     (let ((d12 (i+ (i* d1 10) d2)))
		       (when (or (i= d12 0)
				 (i= d12 99)
				 (and (i<= d12 12) (i> d12 0)))
			 (iincf point 2)
			 (setq month d12)
			 t))))))
	     (parse-month3 ()
	       (when (i<= point (i- end 3))
		 (let ((c1 (char string point))
		       (c2 (char string (i+ point 1)))
		       (c3 (char string (i+ point 2))))
		   (when (and (ascii-letter-p c1) (ascii-letter-p c2) (ascii-letter-p c3))
		     (loop for m fixnum from 0 below 12
			   as s = (svref #("JAN" "FEB" "MAR" "APR" "MAY" "JUN"
					   "JUL" "AUG" "SEP" "OCT" "NOV" "DEC") m)
			   when (and (char-equal c1 (char s 0))
				     (char-equal c2 (char s 1))
				     (char-equal c3 (char s 2)))
			   do (iincf point 3)
			   (setq month (i+ m 1))
			   (return t))))))
	     (parse-year2 ()
	       (when (i<= point (i- end 2))
		 (let ((d1 (ascii-digit-p (char string point)))
		       (d2 (ascii-digit-p (char string (i+ point 1)))))
		   (when (and d1 d2)
		     (iincf point 2)
		     (setq year (i+ (i* d1 10) d2))
		     t))))
	     (parse-year4 ()
	       (when (i<= point (i- end 4))
		 (let ((d1 (ascii-digit-p (char string point)))
		       (d2 (ascii-digit-p (char string (i+ point 1))))
		       (d3 (ascii-digit-p (char string (i+ point 2))))
		       (d4 (ascii-digit-p (char string (i+ point 3)))))
		   (when (and d1 d2 d3 d4)
		     (let ((y (i+ (i+ (i* d1 1000) (i* d2 100))
				  (i+ (i* d3 10) d4))))
		       (when (or (i= y 0)
				 (i= y 9999)
				 (and (i>= y 1300)
                                      (i<= y 2600))) ; year must be after $time-in-distant-future
			 (iincf point 4)
			 (setq year y)
			 t)))))))
      (declare (dynamic-extent #'punct
			       #'parse-day1 #'parse-day2
			       #'parse-month1 #'parse-month2 #'parse-month3
			       #'parse-year2 #'parse-year4))
      (macrolet ((end ()
		   '(when (or (i= point end)
			   (char-equal (char string point) #\H)
			   (char-equal (char string point) #\T)     ; XML dateTime type
			   (eql (char string point) #\space))
		     (unless (i= point end)
		       ;; Parse the time
		       (multiple-value-setq (seconds minutes hours)
			 (explode-local-tofd string
					     :start (i+ point 1) :end end)))
		     (return-from explode-local-time
                       (values seconds minutes hours day month (or year default-year)))))
		 (and+ (&rest args)
		   `(block end-of-and
		      (let ((p point))
			,@(mapcar (lambda (arg)
				    `(unless ,arg
				       (setq point p)
				       (return-from end-of-and nil)))
				  args)
			t))))
	(or (and+ (or (parse-day2) (parse-day1))
		  (punct)
		  (parse-month3)
		  (or (end)
		      (and+ (punct)
			    (or (parse-year4) (parse-year2))
			    (end))))
	    (and+ (or (and+ (parse-year4) (punct)) t)
		  (parse-month3)
		  (punct)
		  (or (parse-day2) (parse-day1))
		  (end))
	    (and+ (parse-day2) (punct) (parse-month2) (punct) (parse-year4) (end))
	    (and+ (parse-day1) (punct) (parse-month2) (punct) (parse-year4) (end))
	    (and+ (or (parse-day2) (parse-day1)) (punct nil)
		  (or (parse-month2) (parse-month1)) (punct nil)
		  (parse-year4) (end))
	    (and+ (parse-year4) (punct) (parse-month2) (punct)
		  (or (parse-day2) (parse-day1)) (end))
	    (and+ (parse-year4) (punct nil)
		  (or (parse-month2) (parse-month1)) (punct nil)
		  (or (parse-day2) (parse-day1)) (end))
	    (and (i= (length string) 6)	;ddmmyy, *not* mmddyy
		 (every #'ascii-digit-p string)
		 (and+ (parse-day2) (parse-month2) (parse-year2) (end)))
	    (and (i<= (length string) 5) ;mmyy or mm/yy
		 (and+ (parse-month2) (punct) (parse-year2)
		       (setq day (days-per-month month year))
		       (end)))
	    (and (i<= (length string) 7) ;mmyyyy or mm/yyyy
		 (and+ (parse-month2) (punct) (parse-year4)
		       (setq day (days-per-month month year))
		       (end))))))))


(defun explode-iso8601-date (string
			&key (start 0) (end (length string)))
  "Convert an ISO-8601 date string into its numerical components.
  'string' may have one of precisely must be YYYY-MM-DD syntax.
  Return values are:
      <day> <month> <year>
  If parsing fails, all NILs are returned."
  (check-types (string string)
	       (fixnum start end))
  ;; basic syntax check:  year, month, day, hour, minute pieces
  (unless (and (eql (- end start) 10)
               (eql (char string (i+ start 4)) #\-)
               (eql (char string (i+ start 7)) #\-))
    (return-from explode-iso8601-date
      (values nil nil nil)))
  (flet ((parse2 (string posn)
	   (let ((d1 (ascii-digit-p (char string posn)))
		 (d0 (ascii-digit-p (char string (i+ posn 1)))))
	     (and d1 d0
		  (i+ (i* d1 10) d0))))
	 (parse4 (string posn)
	   (let ((d3 (ascii-digit-p (char string posn)))
		 (d2 (ascii-digit-p (char string (i+ posn 1))))
		 (d1 (ascii-digit-p (char string (i+ posn 2))))
		 (d0 (ascii-digit-p (char string (i+ posn 3)))))
	     (and d3 d2 d1 d0
		  (+ (i* d3 1000) (i* d2 100) (i* d1 10) d0)))))
    (declare (dynamic-extent #'parse2 #'parse4))
    (let ((year   (parse4 string start))
	  (month  (parse2 string (i+ start 5)))
	  (day    (parse2 string (i+ start 8))))
      ;; year, month, day, hour, minute are the bare-bones pieces.
      (unless (and year month day)
	(return-from explode-iso8601-date
	  (values nil nil nil nil nil nil nil nil nil nil)))
        (values day month year))))

(defun explode-iso8601-tofd (string
			     &key (start 0) (end (length string)))
  "Convert an ISO-8601 tofd string into its numerical components.
  'string' may have one of these syntaxes:

     hh:mm[:ss[.ffff...]]Z
     hh:mm[:ss[.ffff...]]Saa[:bb]
     hh:mm[:ss[.ffff...]]

  where:   hh = hour
           mm = minute
           ss = second
        fff.. = fractional seconds
            Z = literal 'Z', meaning a UTC time
            S = timezone offset sign:  either '-' or '+'
           aa = timezone offset hour
           bb = timezone offset minute

  The return values are the components:
      <seconds>             [optional]
      <minutes> <hours>     [required]
      <tz-offset minutes> <tz-offset hours> <tz-offset sign> [optional]
      <seconds fraction>    [optional]

  All values are integers, except for <seconds fraction> (a rational),
   or missing optional values (NIL).

  <tz-offset sign> is zero iff the timestring is in the UTC 'Z' form;
   if any timezone is specified (even 00:00), it will be +1 or -1.
   <tz-offset minutes/hours> are zero if <tz-offset sign> is zero.
   All tz-offset values are NIL if timezone fields are not supplied.

  If parsing fails, all NILs are returned."
  (check-types (string string)
	       (fixnum start end))
  ;; basic syntax check:  hour, minute pieces
  (unless (eql (char string (i+ start 2)) #\:)
    (return-from explode-iso8601-tofd
      (values nil nil nil nil nil nil nil)))
  (let* ((size   (- end start))
         (has-seconds (and (> size 5)
                           (eql (char string (i+ start 5)) #\:)))
         (has-fraction (and has-seconds
                            (> size 8)
                            (eql (char string (i+ start 8)) #\.)))
         (end-of-tofd (cond (has-fraction (or (position-if-not #'ascii-digit-p
                                                               string
                                                               :start (i+ start 9)
                                                               :end end)
                                              end))
                            (has-seconds (i+ start 8))
                            (t (i+ start 5))))
         (tz-sign (and (> end end-of-tofd)
                       (cond ((eql (char string end-of-tofd) #\Z) 0)
                             ((eql (char string end-of-tofd) #\+) +1)
                             ((eql (char string end-of-tofd) #\-) -1)
                             (t (return-from explode-iso8601-tofd
                                  (values nil nil nil nil nil nil nil)))))))
    (flet ((parse2 (string posn)
	     (let ((d1 (ascii-digit-p (char string posn)))
		   (d0 (ascii-digit-p (char string (i+ posn 1)))))
	       (and d1 d0
		    (i+ (i* d1 10) d0))))
	   (parse-frac (string start end)
             (loop with place-value = (/ 1 10)
                   for spot from start below end
                   summing (let ((d (ascii-digit-p (char string spot))))
                             (assert d () "~A is not a digit as expected" (char string spot))
                             (* place-value d))
                   doing (setq place-value (/ place-value 10)))))
      (declare (dynamic-extent #'parse2 #'parse-frac))
      (let ((hour   (parse2 string start))
	    (minute (parse2 string (i+ start 3)))
            (second (and has-seconds
                         (parse2 string (i+ start 6))))
            (fraction (and has-fraction
                           (parse-frac string (i+ start 9) end-of-tofd))))
	;; hour, minute are the bare-bones pieces.
        (unless (and hour minute)
          (return-from explode-iso8601-tofd
            (values nil nil nil nil nil nil nil)))
        (multiple-value-bind (tz-hour tz-minute)
            (cond ((not tz-sign)   (values nil nil))
                  ((zerop tz-sign) (values 0 0))
                  ((and (i= (i+ end-of-tofd 6) end)
			(eql (char string (i+ end-of-tofd 3)) #\:))
		   (values (parse2 string (i+ end-of-tofd 1))
			   (parse2 string (i+ end-of-tofd 4))))
		  ((i= (i+ end-of-tofd 3) end)
		   (values (parse2 string (i+ end-of-tofd 1))
			   nil))
		  (t
                   (return-from explode-iso8601-tofd nil)))
          (values second minute hour
                  tz-minute tz-hour tz-sign
                  fraction))))))

(defun explode-iso8601 (string
			&key (start 0) (end (length string)))
  "Convert an ISO-8601 date+tofd string into its numerical components.
  'string' may have one of these syntaxes:

     YYYY-MM-DDThh:mm[:ss[.ffff...]]Z
     YYYY-MM-DDThh:mm[:ss[.ffff...]]Saa[:bb]
     YYYY-MM-DDThh:mm[:ss[.ffff...]]

  where: YYYY = year
           MM = month
           DD = day/date
            T = literal 'T'
           hh = hour
           mm = minute
           ss = second
        fff.. = fractional seconds
            Z = literal 'Z', meaning a UTC time
            S = timezone offset sign:  either '-' or '+'
           aa = timezone offset hour
           bb = timezone offset minute

  The return values are the components:
      <seconds>             [optional]
      <minutes> <hours>     [required]
      <day> <month> <year>  [required]
      <tz-offset minutes> <tz-offset hours> <tz-offset sign> [optional]
      <seconds fraction>    [optional]

  All values are integers, except for <seconds fraction> (a rational),
   or missing optional values (NIL).

  <tz-offset sign> is zero iff the timestring is in the UTC 'Z' form;
   if any timezone is specified (even 00:00), it will be +1 or -1.
   <tz-offset minutes/hours> are zero if <tz-offset sign> is zero.
   All tz-offset values are NIL if timezone fields are not supplied.

  If parsing fails, all NILs are returned."
  (check-types (string string)
	       (fixnum start end))
  ;; basic syntax check:  year, month, day, hour, minute pieces
  (unless (and (>= (- end start) 16)
	       (eql (char string (i+ start 10)) #\T)
               (eql (char string (i+ start 13)) #\:))
    (return-from explode-iso8601
      (values nil nil nil nil nil nil nil nil nil nil)))
  (multiple-value-bind (day month year)
      (explode-iso8601-date string :start start :end (+ start 10))
    (multiple-value-bind (second minute hour
				 tz-minute tz-hour tz-sign
				 fraction)
	(explode-iso8601-tofd string :start (+ start 11) :end  end)
      ;; year, month, day, hour, minute are the bare-bones pieces.
      (unless (and year month day hour minute)
	(return-from explode-iso8601
	  (values nil nil nil nil nil nil nil nil nil nil)))
      (values second minute hour day month year
	      tz-minute tz-hour tz-sign
	      fraction))))

(defun parse-duration (string &key (start 0) (end (length string)))
  "Convert the duration 'string' to a number of seconds.  'string' may take
  one of the following forms:
    HH:MM   HH:MM:SS   HHMM   HHMMSS
   +HH:MM  +HH:MM:SS  +HHMM  +HHMMSS
   -HH:MM  -HH:MM:SS  -HHMM  -HHMMSS
  The sign of the result reflects the sign of the duration."
  (check-types (string string)
	       (integer start end))
  (let ((sign  +1))
    (cond ((char-equal (char string start) #\+)
	   (iincf start))
	  ((char-equal (char string start) #\-)
	   (setf sign -1)
	   (iincf start)))
    (multiple-value-bind (seconds minutes hours)
	(explode-local-tofd string :start start :end end)
      (if (and minutes hours)
        (* sign (+ (* (or seconds 0) $1second)
                   (* minutes $1minute)
                   (* hours $1hour)))
        NIL))))


(defun parse-local-tofd (string &key (start 0) (end (length string)))
  "Parse a string (or substring) as a local tofd.
   Valid formats are:  HHMM   HHMMSS
                       HH:MM  HH:MM:SS
   Returns the number of seconds since (local) midnight."
  (check-types (string string)
	       (fixnum start end))
  (multiple-value-bind (seconds minutes hours)
      (explode-local-tofd string :start start :end end)
    (if (and minutes hours)
      (+ (or seconds 0) (* minutes $1minute) (* hours $1hour))
      NIL)))


;;!!!TZ Need a better way to deal with inferring a missing YEAR.

(defun parse-local-time (string &key (start 0) (end (length string))
			             (guess-year :utc-future)
                                     (guess-year-offset 0)
                                     (guess-century :unix-y2k))

  "Parse a (sub)string as a local time (date+tofd).

  STRING is the time string to parse.

  START and END are optional bounds.

  GUESS-YEAR specifies how the year should be inferred if it is
  missing from STRING.  The options for GUESS-YEAR are:

        A number - The given number is used when STRING does not specify a year.
     :utc-future - Choose the year such that the time is in the future (or same date), relative to
                   the current printed UTC date.  [default]
    :utc-current - Use the year from the current printed UTC date.
             NIL - Return NIL if no year is specified.
       :utc-past - Choose the year such that the time is in the past (or same date), relative to the
                   current printed UTC date.

  GUESS-YEAR-OFFSET specifies an offset (in seconds) which is added
  to the current UTC time before inferring the year for a yearless
  STRING.  It is applicable only when GUESS-YEAR is :UTC-FUTURE or
  :UTC-PAST.  GUESS-YEAR-OFFSET defaults to zero.

  GUESS-CENTURY specifies how the year should be interpreted if
  it is specified as (exactly) two digits.  The options are:

    :utc-nearest - Choose a date within 50 years of the current UTC date.
       :utc-past - Choose the latest century giving a past UTC date.
     :utc-future - Choose the earliest century not giving a past UTC date.
       :unix-y2k - Calls %year-from-2digit-year, resulting in a year
                   in the range 1970 to 2069 [default].

  The function returns a local datetime as an integer-time, or NIL."
  
  (check-types (string string)
	       (fixnum start end))
  (multiple-value-bind (seconds minutes hours day month year)
      (explode-local-time string :start start :end end)
    (when (and minutes hours day month
               (>= month 1) (<= month 12)
               (> day 0))
      (cond
       ((null year)
        (setq year (cond
                     ((eq guess-year :utc-future)
                      (multiple-value-bind (curday curmon curyear)
                          (decode-integer-date (i+ (current-time-utc) guess-year-offset))
                        (cond ((i< curmon month) curyear)
                              ((and (i= curmon month)
                                    (i<= curday day)) curyear)
                              (t (i+ curyear 1)))))
                     ((eq guess-year :utc-past)
                      (multiple-value-bind (curday curmon curyear)
                          (decode-integer-date (i+ (current-time-utc) guess-year-offset))
                        (cond ((i< month curmon) curyear)
                              ((and (i= curmon month)
                                    (i<= day curday)) curyear)
                              (t (i- curyear 1)))))
                     ((eq guess-year :utc-current)
                      (nth-value 2 (decode-integer-date (current-time-utc))))
                     ((numberp guess-year) guess-year)
                     (t (assert (null guess-year) ()
                                "Invalid :guess-year option '~A'" guess-year)))))
       ((i< year 100)
        (setq year (multiple-value-bind (curday curmon curyear)
                       (decode-integer-date (current-time-utc))
                     (let* ((adjusted-curyear (cond ((i< curmon month) curyear)
                                                     ((and (i= curmon month)
                                                           (i<= curday day)) curyear)
                                                     (t (i+ curyear 1))))
                            ;; Fenceposting: year-offset 0 denotes a date between now and a year hence.
                            (year-offset (ecase guess-century
                                           (:unix-y2k
                                            (i- (%year-from-2digit-year year) adjusted-curyear))
                                           (:utc-nearest
                                            (i+ -50 (mod (i- year adjusted-curyear 50) 100)))
                                           (:utc-past
                                            (i+ -100 (mod (i- year adjusted-curyear) 100)))
                                           (:utc-future
                                            (mod (i- year adjusted-curyear) 100)))))
                       (i+ adjusted-curyear year-offset))))))

      (when (and year
                 (<= day (days-per-month month year)))
	(encode-integer-time (or seconds 0) minutes hours day month year)))))


(defun parse-local-date (string &key (start 0) (end (length string))
			             (guess-year :utc-future)
                                     (guess-year-offset 0)
                                     (guess-century :unix-y2k))

  "Parse a (sub)string as a local date, neglecting tofd components.

  STRING is the time string to parse.

  START and END are optional bounds.

  GUESS-YEAR specifies how the year should be inferred if it is
  missing from STRING.  The options for GUESS-YEAR are:

        a number - The given number is used when STRING does not
                   specify a year.
     :utc-future - Choose the year such that the time is in the
                   future, relative to the current printed UTC
                   time.  [default]
    :utc-current - Use the year from the current printed UTC time.
             NIL - Return NIL if no year is specified.
       :utc-past - Choose the year such that the time is in the
                   past, relative to the current printed UTC time.

  GUESS-YEAR-OFFSET specifies an offset (in seconds) which is added
  to the current UTC time before inferring the year for a yearless
  STRING. It is applicable only when GUESS-YEAR is :UTC-FUTURE or
  :UTC-PAST.  GUESS-YEAR-OFFSET defaults to zero.

  GUESS-CENTURY specifies how the year should be interpreted if
  it is specified as (exactly) two digits.  The options are:

    :utc-nearest - Choose a date within 50 years of the current UTC date.
       :utc-past - Choose the latest century giving a past UTC date.
     :utc-future - Choose the earliest century not giving a past UTC date.
       :unix-y2k - Calls %year-from-2digit-year, resulting in a year
                   in the range 1970 to 2069 [default].

  The function returns a local date represented as an integer-time, or NIL."
  
  (let ((time (parse-local-time string :start start :end end
                                :guess-year guess-year
                                :guess-year-offset guess-year-offset
                                :guess-century guess-century)))
    (and time (local-date-only time))))


(defun parse-iso8601-date (string &key (start 0) (end (length string)))
  "Convert an ISO-8601 date (sub)string into an integer-date.
   Must be in YYYY-MM-DD syntax.
   If parsing fails, NIL is returned."
  (check-types (string string)
	       (fixnum start end))
  (multiple-value-bind (day month year)
      (explode-iso8601-date string :start start :end end)
    (when (and day month year)
      (encode-integer-date day month year))))

(defun parse-iso8601-utc (string &key (start 0) (end (length string)))
  "Convert an ISO-8601 date+tofd (sub)string into a UTC integer-time.
  (For allowed syntaxes, see 'explode-iso8601'.)
  Any fractional seconds are truncated.
  If parsing fails, NIL is returned."
  (check-types (string string)
	       (fixnum start end))
  (multiple-value-bind (second minute hour day month year
		        tz-minute tz-hour tz-sign)
      (explode-iso8601 string :start start :end end)
    (when (and minute hour day month year)
      (if (and tz-sign tz-hour)
	(- (encode-integer-time (or second 0) minute hour day month year)
	   (i* tz-sign (i+ (* $1hour tz-hour) (* $1minute (or tz-minute 0)))))
	(encode-integer-time (or second 0) minute hour day month year)))))

(defun parse-iso8601-local (string &key (start 0) (end (length string)))
  "Convert an ISO-8601 date+tofd (sub)string into an integer-time.
  (For allowed syntaxes, see 'explode-iso8601'.)
  Any fractional seconds are truncated.
  Any time zone specification is ignored.
  If parsing fails, NIL is returned."
  (check-types (string string)
	       (fixnum start end))
  (multiple-value-bind (second minute hour day month year
		        tz-minute tz-hour tz-sign)
      (explode-iso8601 string :start start :end end)
    (declare (ignore tz-minute tz-hour tz-sign))
    (when (and minute hour day month year)
      (encode-integer-time (or second 0) minute hour day month year))))

(defun parse-iso8601-tofd-zoned (string &key (start 0) (end (length string)))
  "Convert an ISO-8601 tofd (sub)string into a zoned-time.
  (For allowed syntaxes, see 'explode-iso8601-tofd'.)
  Any fractional seconds are truncated.
  If parsing fails, NIL is returned."
  (check-types (string string)
	       (fixnum start end))
  (multiple-value-bind (second minute hour
                        tz-minute tz-hour tz-sign)
      (explode-iso8601-tofd string :start start :end end)
    (when (and minute hour tz-sign tz-hour)
      (let ((offset (* tz-sign (+ (* $1hour tz-hour) (* $1minute (or tz-minute 0))))))
	(make-instance 'zoned-time
	  :utc (- (encode-integer-time (or second 0) minute hour
				       01 01 2000)
		  offset)
	  :tzoffset offset)))))

(defun parse-iso8601-zoned (string &key (start 0) (end (length string)))
  "Convert an ISO-8601 date+tofd (sub)string into a zoned-time.
  (The allowed syntax is YYYY-MM-DDThh:mm[:ss[.ffff...]]Saa:bb;
  see 'explode-iso8601' for an explanation.)
  Any fractional seconds are truncated.
  If parsing fails, NIL is returned."
  (check-types (string string)
	       (fixnum start end))
  (multiple-value-bind (second minute hour day month year
                        tz-minute tz-hour tz-sign)
      (explode-iso8601 string :start start :end end)
    (when (and minute hour day month year tz-sign tz-hour tz-minute)
      (let ((offset (* tz-sign (+ (* $1hour tz-hour) (* $1minute tz-minute)))))
	(make-instance 'zoned-time
	  :utc (- (encode-integer-time (or second 0) minute hour
				       day month year)
		  offset)
	  :tzoffset offset)))))

(defun merge-date-and-tofd-zoned (date zul &optional (local-date-offset 0))
  "Combines an integer-date and a the time portion of a zoned-time,
   and optionally a day offset for the local-date."
  (let* ((tzoffset (tzoffset zul))
	 (local-time (+ (utc-time zul) tzoffset)))
    (make-instance 'zoned-time
      ;; combine local date with local time, then convert back to UTC
      :utc (- (+ (local-date-only date) (* local-date-offset $24hours) (local-tofd-only local-time)) tzoffset)
      :tzoffset tzoffset)))

;;; Time and date printing

(defun write-duration (duration &optional (stream *standard-output*)
		       &key (format :hh-mm)
		            (show-plus nil))
  ;; Note: default format :hh-mm ONLY valid for durations under 100 hrs
  ;; The current code will use cryptic ASCII characters under 790 hrs or so,
  ;; then may use unicode characters under a few million hours, then overflow
  ;; and cause a Lisp error. This suggests a code review at some point,
  ;; including a robustification of many such functions and their callers.
  ;; I filed bug 46257 for such a review. --fare 2008-08-15
  (let ((sign (cond ((< duration 0) "-")
		    (show-plus "+")
		    (t nil)))
	(duration (abs duration)))
    (multiple-value-bind (hrs rem) (floor duration $1hour)
      (multiple-value-bind (mins secs) (floor rem $1minute)
	(macrolet ((write-nn (i)
		     `(multiple-value-bind (d2 d1) (floor ,i 10)
		       (write-char (code-char (i+ d2 #.(char-code #\0))) stream)
		       (write-char (code-char (i+ d1 #.(char-code #\0))) stream))))
	  (when sign
	    (write-string sign stream))
	  (ecase format
	    (:hhmm ;ONLY valid for durations under 100 hrs
	     (write-nn hrs)
	     (write-nn mins))
	    (:hh-mm ;ONLY valid for durations under 100 hrs
	     (write-nn hrs)
	     (write-string ":" stream)
	     (write-nn mins))
	    (:h*-mm
             (format stream "~2,'0D" hrs)
	     (write-string ":" stream)
	     (write-nn mins))
	    (:hh-mm-ss ;ONLY valid for durations under 100 hrs
	     (write-nn hrs)
	     (write-string ":" stream)
	     (write-nn mins)
	     (write-string ":" stream)
	     (write-nn secs))
	    (:h*-mm-ss
             (format stream "~D" hrs)
	     (write-string "h" stream)
	     (write-nn mins)
	     (write-string "m" stream)
	     (write-nn secs)
	     (write-string "s" stream))
            )))))
  nil)


(defun write-exploded-date (day month year weekday
			    &optional (stream *standard-output*)
			    &key (format :yyyy-mm-dd)
                                 (show-weekday nil)
                                 (use-uppercase t))
  "Write an exploded local date to a stream; input is the component fixnums
    day, month, year, weekday.
   If 'weekday' is not NIL, ':show-weekday' specifies where to write an
    English weekday:
       :before, :before-dash - before the date
       :after,  :dash-after  - after the date
           NIL - do not display weekday
   If 'use-uppercase' is NIL, mixed-case is used for English months and
    weekdays.

   Possible output formats are:

    English Month:   :ddmth        '07JAN'
                     :ddmthyy      '07JAN99'
                     :ddmthyyyy    '07JAN1999'
                     :dd-mth-yyyy  '07-JAN-1999'

    Numeric Month:   :ddmmyy       '070199'
                     :mmddyy       '010799'
                     :ddmmyyyy     '07011999'
                     :yyyymmdd     '19990107'
                     :yymmdd       '990107'
                     :yyyy-mm-dd   '1999-01-07'
                     :nu/mm/yy     '01/99'
                     :nu/mm/yyyy   '01/1999'
                     :nu/mmyy      '0199'

   Return value is NIL."
  (check-types (fixnum day month year)
	       ((or fixnum null) weekday))
  (macrolet ((write-nn (i)
	       `(multiple-value-bind (d2 d1) (floor ,i 10)
		 (write-char (code-char (i+ d2 #.(char-code #\0))) stream)
		 (write-char (code-char (i+ d1 #.(char-code #\0))) stream)))
	     (write-mth (m)
	       `(write-string (svref (if use-uppercase
					 $uppercase-months
					 $mixedcase-months) (i- ,m 1)) stream))
	     (write-weekday-before ()
	       `(when (and weekday (or (eq show-weekday :before) (eq show-weekday :before-dash)))
		 (write-string (svref (if use-uppercase
					  $uppercase-days-ws
					  $mixedcase-days) weekday) stream)
                 (when (eq show-weekday :before-dash)
                   (write-char #\- stream))))
	     (write-weekday-after ()
	       `(when (and weekday (or (eq show-weekday :after) (eq show-weekday :dash-after)))
                 (when (eq show-weekday :dash-after)
                   (write-char #\- stream))
		 (write-string (svref (if use-uppercase
					  $uppercase-days-ws
					  $mixedcase-days) weekday) stream))))
    (ecase format
      ;; English Month formats
      (:ddmth				;DDMTH
       (write-weekday-before)
       (write-nn day)
       (write-mth month)
       (write-weekday-after))
      (:ddmthyy				;DDMTHYY
       (write-weekday-before)
       (write-nn day)
       (write-mth month)
       (write-nn (mod year 100))
       (write-weekday-after))
      (:ddmthyyyy			;DDMTHYYYY
       (write-weekday-before)
       (write-nn day)
       (write-mth month)
       (write-integer year stream)
       (write-weekday-after))
      (:dd-mth-yyyy			;DD-MTH-YYYY
       (write-weekday-before)
       (write-nn day)
       (write-string "-" stream)
       (write-mth month)
       (write-string "-" stream)
       (write-integer year stream)
       (write-weekday-after))
      ;; Numeric Month formats
      (:ddmmyy				;DDMMYY
       (write-weekday-before)
       (write-nn day)
       (write-nn month)
       (write-nn (mod year 100))
       (write-weekday-after))
      (:mmddyy				;MMDDYY
       (write-weekday-before)
       (write-nn month)
       (write-nn day)
       (write-nn (mod year 100))
       (write-weekday-after))
      (:ddmmyyyy			;DDMMYYYY
       (write-weekday-before)
       (write-nn day)
       (write-nn month)
       (write-integer year stream)
       (write-weekday-after))
      (:mmddyyyy			;MMDDYYYY
       (write-weekday-before)
       (write-nn month)
       (write-nn day)
       (write-integer year stream)
       (write-weekday-after))
      (:yyyymmdd			;YYYYMMDD - for TCN reporting
       (write-weekday-before)
       (write-integer year stream)
       (write-nn month)
       (write-nn day)
       (write-weekday-after))
      (:yyyymm                         ;YYYYMM - for BIDT reporting
       (write-weekday-before)
       (write-integer year stream)
       (write-nn month)
       (write-weekday-after))
      (:yymmdd			       ;YYMMDD - used in edifact's UNB+IATA... header
       (write-nn (mod year 100))
       (write-nn month)
       (write-nn day))
      (:yyyy-mm-dd			;YYYY-MM-DD
       (write-weekday-before)
       (write-integer year stream)
       (write-string "-" stream)
       (write-nn month)
       (write-string "-" stream)
       (write-nn day)
       (write-weekday-after))
      (:yyyy-mm				;YYYY-MM
       (write-integer year stream)
       (write-string "-" stream)
       (write-nn month))
      ;; was :mmyy
      (:nu/mm/yy			;MM/YY, for expiration dates
       (write-nn month)
       (write-string "/" stream)
       (write-nn (mod year 100)))
      (:nu/mm/yyyy			;MM/YYYY, for expiration dates
       (write-nn month)
       (write-string "/" stream)
       (write-integer year stream))
      ;; Some special-purpose formats
      (:d				;With :hhmm below, for AIRIMP
       (write-nn day))
      ;; was :ccexp
      (:nu/mmyy				;MMYY, for expiration dates
       (write-nn month)
       (write-nn (mod year 100)))
      )))


(defun write-exploded-tofd (seconds minutes hours
			    &optional (stream *standard-output*)
			    &key (format :hh-mm))
  "Write an exploded local tofd to a stream; input is the component
    seconds, minutes, and hours.
   Possible output formats are:
      :hh-mm = HH:MM  :hh-mm-ss = HH:MM:SS   :hh-mm-ap = HH:MMAM, HH:MMPM
      :_h-mm =  H:MM                         :_h-mm-ap =  H:MMa,   H:MMp
      :hhmm  = HHMM   :hhmmss   = HHMMSS     :hhmmap   = HHMMA, HHMMP
                                             :_hmmap   =  HMMA,  HMMP

   The \"_h\" formats use a leading space instead of a leading zero.
   Return value is NIL."
  (check-types (fixnum seconds minutes hours))
  ;; Avoid using 'format' since it conses so much
  (flet ((write-nn (i)
	   (declare (type fixnum i))
	   (multiple-value-bind (d2 d1) (floor i 10)
	       (write-char (code-char (+ d2 #.(char-code #\0))) stream)
	       (write-char (code-char (+ d1 #.(char-code #\0))) stream)))
         (write-_n (i) ; space padded to two digits
           (declare (type fixnum i))
           (multiple-value-bind (d2 d1) (floor i 10)
             (write-char (if (> d2 0)
                             (code-char (i+ d2 #.(char-code #\0)))
                             #\SPACE) stream)
             (write-char (code-char (i+ d1 #.(char-code #\0))) stream)))
	 (ampm-hours (hours)
	   (declare (type fixnum hours))
	   (cond ((= hours 0) 12)
		 ((= hours 24) 0)
		 ((> hours 12) (- hours 12))
		 (t hours))))
    (declare (inline write-nn ampm-hours)
	     (dynamic-extent #'write-nn #'ampm-hours))
    (ecase format
      (:hh-mm				;hh:mm
       (write-nn hours)
       (write-string ":" stream)
       (write-nn minutes))
      (:_h-mm				;_h:mm
       (write-_n hours)
       (write-string ":" stream)
       (write-nn minutes))
      (:hh-mm-ss			;hh:mm:ss
       (write-nn hours)
       (write-string ":" stream)
       (write-nn minutes)
       (write-string ":" stream)
       (write-nn seconds))
      (:hhmm				;hhmm
       (write-nn hours)
       (write-nn minutes))
      (:hhmmss				;hhmmss
       (write-nn hours)
       (write-nn minutes)
       (write-nn seconds))
      (:hhmmap				;hhmmA or hhmmP
       (write-nn (ampm-hours hours))
       (write-nn minutes)
       (write-string (if (>= hours 12) "P" "A") stream))
      (:_hmmap
       (write-_n (ampm-hours hours))
       (write-nn minutes)
       (write-string (if (>= hours 12) "P" "A") stream))
      (:hh-mm-ap			;hh:mmAM or hh:mmPM
       (write-nn (ampm-hours hours))
       (write-string ":" stream)
       (write-nn minutes)
       (write-string (if (>= hours 12) " PM" " AM") stream))
      (:_h-mm-ap			;_H:MMa or _H:MMp
       (write-_n (ampm-hours hours))
       (write-string ":" stream)
       (write-nn minutes)
       (write-char (if (>= hours 12) #\p #\a) stream))
      (:h-mm-ap-word ;H:MMa or HH:MMa or H:MMp or midnight or noon or ...
       (cond
         ((and (i= hours 0) (i= minutes 0))  (write-string "midnight" stream))
         ((and (i= hours 12) (i= minutes 0)) (write-string "noon" stream))
         (t
          (write-integer (ampm-hours hours) stream)
          (write-string ":" stream)
          (write-nn minutes)
          (write-char (if (>= hours 12) #\p #\a) stream))))
      ))
  nil)


(defun write-zoned-time (zul &optional (stream *standard-output*)
			 &key (date-as :yyyy-mm-dd)
			      (time-as :hh-mm time-as-supplied-p)
			      (show-weekday nil)
			      (as-utc nil)
			      (show-timezone t show-tz-supplied-p)
			      (use-uppercase t))
  "Write the zoned-time ZUL to the STREAM.  If DATE-AS is specified as :ISO8601,
   use ISO-8601 format with possible variations as specified by DATE-AS and
   TIME-AS.  AS-UTC, if non-NIL, indicates that the the time should be considered
   as being UTC, with no timezone information output.  The DATE-AS and TIME-AS
   arguments accept format arguments as accepted by WRITE-EXPLODED-DATE and
   WRITE-EXPLODED-TOFD."
  (check-types (zoned-time zul))
  (multiple-value-bind (seconds minutes hours day month year weekday)
      (if as-utc
	(decode-integer-time (utc-time zul))
	(decode-integer-time (local-time zul)))

    (case date-as
      (:iso8601
       (write-exploded-date day month year weekday stream
			    :format :yyyy-mm-dd
			    :show-weekday nil
			    :use-uppercase use-uppercase)
       (write-string "T" stream)
       (unless time-as-supplied-p (setf time-as :hh-mm-ss))
       (write-exploded-tofd seconds minutes hours stream
			    :format time-as)
       (if as-utc
	 (write-string "Z" stream)
	 (write-duration (tzoffset zul) stream
			 :format :hh-mm :show-plus t)))
      (otherwise
       (when date-as
	 (write-exploded-date day month year weekday stream
			      :format date-as
			      :show-weekday show-weekday
			      :use-uppercase use-uppercase)
         (when time-as
           (write-string " " stream)))
       (when time-as
	 (write-exploded-tofd seconds minutes hours stream
			      :format time-as))
       (when (and (or show-tz-supplied-p time-as)
		  show-timezone)
	 (write-duration (tzoffset zul) stream
			 :format (case time-as
				   ((:hhmm :hhmmss :hhmmap) :hhmm)
				   (otherwise :hh-mm))
                         :show-plus t)))
      ))
  nil)

(defun cl-user::zoned-time (stream zul colon-p atsign-p)
  "Format function for displaying a zoned time, with defaulted write-zoned-time parameters."
  (declare (ignore colon-p atsign-p))
  (write-zoned-time zul stream))


(defun write-local-tofd (tofd &optional (stream *standard-output*)
		         &key (format :hh-mm))
  "Write a local tofd to a stream.  'tofd' is an integer-tofd,
    i.e. seconds from midnight.
   Possible output formats are:
      :hh-mm = HH:MM  :hh-mm-ss = HH:MM:SS   :hh-mm-ap = HH:MMAM, HH:MMPM
      :hhmm  = HHMM   :hhmmss   = HHMMSS     :hhmmap   = HHMMA, HHMMP
   Return value is NIL."
  (declare (type integer-tofd tofd)
	   (type stream stream)
	   (type keyword format))
  (multiple-value-bind (seconds minutes hours)
      (decode-integer-tofd tofd)
    (write-exploded-tofd seconds minutes hours stream :format format)))


(defun write-local-date (date &optional (stream *standard-output*)
			 &key (format :yyyy-mm-dd)
			      (show-weekday nil)
			      (use-uppercase t))
  "Write the given local integer-date DATE to STREAM according to the FORMAT,
   SHOW-WEEKDAY and USE-UPPERCASE arguments as expected by WRITE-EXPLODED-DATE."
  (multiple-value-bind (day month year weekday)
      (decode-integer-date date)
    (write-exploded-date day month year weekday stream
			 :format format
			 :show-weekday show-weekday
			 :use-uppercase use-uppercase)))

(defun local-date-to-string-format (date format) 
  "Return the given local integer-date DATE as formatted string according to the FORMAT,
   SHOW-WEEKDAY and USE-UPPERCASE arguments as expected by WRITE-EXPLODED-DATE."
  (check-types (integer-time date))
  (with-output-to-string (stream)
    (write-local-date date stream 
      :format format :show-weekday nil :use-uppercase t)))

(defun local-date-to-iso8601-string (date)
  "Return the given local integer-date DATE as a string formatted as YYYY-MM-DDD."
  (local-date-to-string-format date :yyyy-mm-dd))

;; ---???hh I wonder why no error is signaled for unknown FORMAT arguments here?
(defun local-date-to-string (date &optional format)
  "Return the given local integer-date DATE as string.  FORMAT specifies how the
   date should be formatted.

   The following keywords are accepted for FORMAT:

   :DATE-ONLY -> DDMTHYYY
   :BRIEF -> DD-MTH-YYYY HH:MM
   any of (:DDMTH :DDMTHYY :DDMTHYYYY :DD-MTH-YYYY :DDMMYY 
           :MMDDYY :DDMMYYYY :MMDDYYYY :YYYYMMDD :YYYYMM :YYMMDD :YYYY-MM-DD)
    -> whatever WRITE-LOCAL-DATE produces for that format specification.
   For all other values of FORMAT, YYYY-MM-DD HH:MM:SS is produced."
  (check-types (integer-time date))
  (with-output-to-string (stream)
    (case format
      (:date-only
       (write-local-date date stream
			 :format :ddmthyyyy
			 :show-weekday nil
			 :use-uppercase t))
      (:brief
       (write-local-date date stream
			 :format :dd-mth-yyyy
			 :show-weekday nil
			 :use-uppercase t)
       (write-string " " stream)
       (write-local-tofd date stream :format :hh-mm))
      ;;---*** DLD: Shouldn't this take all the formats WRITE-LOCAL-DATE takes?
      ;;---*** msalib: why yes, yes it should
      ((:ddmth :ddmthyy :ddmthyyyy :dd-mth-yyyy :ddmmyy 
	:mmddyy :ddmmyyyy :mmddyyyy 
	:yyyymmdd :yyyymm :yymmdd :yyyy-mm-dd)
       (write-local-date date stream
			 :format format
			 :show-weekday nil
			 :use-uppercase t))
      (otherwise
       (write-local-date date stream
			 :format :yyyy-mm-dd
			 :show-weekday nil
			 :use-uppercase t)
       (write-string " " stream)
       (write-local-tofd date stream :format :hh-mm-ss)))))

(defun local-tofd-to-string (tofd &optional (format :hh-mm))
  "Return the given time of day TOFD as string.  FORMAT may be specified to
   control the format produced by WRITE-LOCAL-TOFD, it defaults to :HH-MM."
  (check-types (integer-time tofd))
  (with-output-to-string (stream)
    (write-local-tofd tofd stream :format format)))

(defun compute-date-change (time1-zul time2-zul)
  "If TIME1-ZUL and TIME2-ZUL are on the same date, return nil, otherwise return
   the days between the two.  See LOCAL-DATE-OFFSET for an explanation of
   the value returned."
  (let ((offset (local-date-offset (local-time time1-zul) (local-time time2-zul))))
    (cond ((izerop offset) nil)
	  (t offset))))
  
(defun date-change-indicator (time1-zul time2-zul)
  "Return a ``date change'' indicator that describes how many days are between
   TIME1-ZUL and TIME2-ZUL.  If they are on the same date, \" \" is returned,
   otherwise it will be a string consisting of a #\+ or a #\- if TIME1-ZUL is
   before or after TIME2-ZUL and the number of days between the two."
  (let ((delta (compute-date-change time1-zul time2-zul)))
    (cond ((null    delta) "  ")
	  ((iminusp delta) (format nil "-~D" (- delta)))
	  ((iplusp  delta) (format nil "+~D" delta)))))

(defun write-iso8601-time (time utc/local time-format &optional (stream *standard-output*))
  (check-types (integer-time time))
  (multiple-value-bind (seconds minutes hours day month year weekday)
      (decode-integer-time time)
    (write-exploded-date day month year weekday stream
			 :format :yyyy-mm-dd
			 :show-weekday nil)
    (write-string "T" stream)
    (write-exploded-tofd seconds minutes hours stream
			 :format time-format)
    (ecase utc/local
      (:utc (write-string "Z" stream))
      (:local nil))
    nil))

(defun write-iso8601-utc (time-utc &optional (stream *standard-output*))
  "Write an integer-time to a stream in ISO-8601 format.
 decode as UTC, write with UTC/ZULU indicator."
  (write-iso8601-time time-utc :utc :hh-mm-ss stream))

(defun write-iso8601-local (time &optional (stream *standard-output*))
  "Write an integer-time to a stream in ISO-8601 format.
 decode as UTC, write as local (w/o ZULU indicator)."
  (write-iso8601-time time :local :hh-mm-ss stream))

(defun time-to-iso8601-string (time utc/local time-format)
  "Create a string in ISO-8601 format from an integer-time."
  (with-output-to-string (stream)
    (write-iso8601-time time utc/local time-format stream)))

(defun utc-to-iso8601-string (time-utc)
  "Create a string in ISO-8601 format from a UTC integer-time."
  (time-to-iso8601-string time-utc :utc :hh-mm-ss))

#|
write-date defaults:  :dd-mmm-yyyy
                      no weekday
                      with time
                      no tzone

:ddmmm        n
:dd-mmm       n
:ddmmm[yy]    n
:ddmmmyy      n
:dd-mmm[-yy]  n
:ddmmmyyyy    n
:dd-mmm-yyyy  maybe write-time, maybe write-tzone
:ddmmyy       n
:ddmmyyyy     n
:dd-mm-yy    maybe write-time, maybe write-tzone  " HH:MM:SS" "[+/-]HH:MM"
:dd-mm-yyyy  maybe write-time, maybe write-tzone
:yyyy-mm-dd  maybe write-time, maybe write-tzone
:mmyy      n
:mmmyyyy   n
:ccexp     n
:ddmmm365  n
|#

(defun cl-user::utc%ISO8601 (stream utc colon-p at-sign-p)
  "Format function for dates, prints UTC in ISO-8601 format to STREAM.  If
   prefixed by a colon in the format specification (e.g. ~:/utc%ISO8601/),
   uppercase letters are used.  UTC may be either an integer-time or a
   zoned-time, but it is always printed as UTC time."
  
  (declare (ignore at-sign-p))
  (check-types ((or integer-time zoned-time) utc))
  (etypecase utc
    (integer-time (write-iso8601-utc utc stream))
    (zoned-time (write-zoned-time utc stream
				  :date-as :iso8601 :as-utc t :use-uppercase (not colon-p)))))

(defun cl-user::utc%DDMTH (stream utc colon-p at-sign-p)
  "Format function for dates, prints UTC in DDMTH format to STREAM.  If prefixed
   by a colon in the format specification (e.g. ~:/utc%DDMTH/), uppercase
   letters are used.  UTC may be either an integer-time or a zoned-time, but it
   is always printed as UTC time."
  (declare (ignore at-sign-p))
  (check-types ((or integer-time zoned-time) utc))
  (etypecase utc
    (integer-time (multiple-value-bind (secs mins hrs day mth yr wkday)
		      (decode-integer-time utc)
		    (declare (ignore hrs secs mins))
		    (write-exploded-date day mth yr wkday stream :format :ddmth :show-weekday nil)))
    (zoned-time (write-zoned-time utc stream
				  :date-as :ddmth :as-utc t :time-as nil :use-uppercase (not colon-p)))))

(defun cl-user::utc%DDMTHYYYY (stream utc colon-p at-sign-p)
  "Format function for dates, prints UTC in DDMTHYYYY format to STREAM.  If
   prefixed by a colon in the format specification (e.g. ~:/utc%DDMTHYYYY/),
   uppercase letters are used.  UTC may be either an integer-time or a
   zoned-time, but it is always printed as UTC time."
  (declare (ignore at-sign-p))
  (check-types ((or integer-time zoned-time) utc))
  (etypecase utc
    (integer-time (multiple-value-bind (secs mins hrs day mth yr wkday)
		      (decode-integer-time utc)
		    (declare (ignore hrs secs mins))
		    (write-exploded-date day mth yr wkday stream :format :ddmthyyyy :show-weekday nil)))
    (zoned-time (write-zoned-time utc stream
				  :date-as :ddmthyyyy :as-utc t :time-as nil :use-uppercase (not colon-p)))))

(defun cl-user::zul%ISO8601 (stream zul colon-p at-sign-p)
  (declare (ignore at-sign-p))
  (check-types (zoned-time zul))
  
  (write-zoned-time zul stream :date-as :iso8601 :use-uppercase (not colon-p)))

(defun cl-user::loc%DDMTH (stream date colon-p at-sign-p)
  (declare (ignore at-sign-p))
  (check-types (integer-time date))
  (write-local-date date stream :format :ddmth :use-uppercase (not colon-p)))

(defun cl-user::loc%DDMTHYY (stream date colon-p at-sign-p)
  (declare (ignore at-sign-p))
  (check-types (integer-time date))
  (write-local-date date stream :format :ddmthyy :use-uppercase (not colon-p)))

(defun cl-user::loc%DDMTHYYYY (stream date colon-p at-sign-p)
  (declare (ignore at-sign-p))
  (check-types (integer-time date))
  (write-local-date date stream :format :ddmthyyyy :use-uppercase (not colon-p)))

(defun cl-user::loc%DDMTHYYWW (stream date colon-p at-sign-p)
  (declare (ignore at-sign-p))
  (check-types (integer-time date))
  (write-local-date date stream :format :ddmthyy :show-weekday :after :use-uppercase (not colon-p)))

;; was DDMMMYYYY
(defun cl-user::loc%DD-MTH-YYYY (stream date colon-p at-sign-p)
  (declare (ignore at-sign-p))
  (check-types (integer-time date))
  (write-local-date date stream :format :dd-mth-yyyy :use-uppercase (not colon-p)))

(defun cl-user::loc%DDMMYY (stream date colon-p at-sign-p)
  (declare (ignore colon-p at-sign-p))
  (check-types (integer-time date))
  (write-local-date date stream :format :ddmmyy))

(defun cl-user::loc%YYMMDD (stream date colon-p at-sign-p)
  (declare (ignore colon-p at-sign-p))
  (check-types (integer-time date))
  (write-local-date date stream :format :yymmdd))

(defun cl-user::loc%WW-YYYY-MM-DD (stream date colon-p at-sign-p)
  (declare (ignore at-sign-p))
  (check-types (integer-time date))
  (write-local-date date stream
		    :format :yyyy-mm-dd :show-weekday :before :use-uppercase (not colon-p)))

#||
(defun cl-user::loc%WW-YYYY-MTH-DD (stream date colon-p at-sign-p)
  (declare (ignore colon-p at-sign-p))
  (check-types (integer-time date))
  (write-local-date date stream :format :yyyy-mth-dd :show-weekday :before))
||#

(defun cl-user::loc%WW-DD-MTH-YYYY (stream date colon-p at-sign-p)
  (declare (ignore at-sign-p))
  (check-types (integer-time date))
  (write-local-date date stream
		    :format :dd-mth-yyyy :show-weekday :before :use-uppercase (not colon-p)))

;; was YYYYMMDDHHMM
(defun cl-user::loc%YYYY-MM-DD-HH-MM-SS (stream date colon-p at-sign-p)
  (declare (ignore colon-p at-sign-p))
  (check-types (integer-time date))
  (write-local-date date stream :format :yyyy-mm-dd)
  (write-string " " stream)
  (write-local-tofd date stream :format :hh-mm-ss))

(defun cl-user::loc%YYYY-MM-DDTHH-MM-SS (stream date colon-p at-sign-p)
  (declare (ignore colon-p at-sign-p))
  (check-types (integer-time date))
  (write-local-date date stream :format :yyyy-mm-dd)
  (write-string "T" stream)
  (write-local-tofd date stream :format :hh-mm-ss))

(defun cl-user::loc%YYYY-MM-DD (stream date colon-p at-sign-p)
  (declare (ignore colon-p at-sign-p))
  (check-types (integer-time date))
  (write-local-date date stream :format :yyyy-mm-dd))

(defun cl-user::loc%YYYY-MM-DDTHH-MM (stream date colon-p at-sign-p)
  (declare (ignore colon-p at-sign-p))
  (check-types (integer-time date))
  (write-local-date date stream :format :yyyy-mm-dd)
  (write-string "T" stream)
  (write-local-tofd date stream :format :hh-mm))

;; was MMYY
(defun cl-user::loc%MM-YY (stream date colon-p at-sign-p)
  (declare (ignore colon-p at-sign-p))
  (check-types (integer-time date))
  (write-local-date date stream :format :nu/mm/yy))

;; was CCEXP
(defun cl-user::loc%MMYY (STREAM date colon-p at-sign-p)
  (declare (ignore colon-p at-sign-p))
  (check-types (integer-time date))
  (write-local-date date stream :format :nu/mmyy))

(defun cl-user::loc%YYYY-MM (STREAM date colon-p at-sign-p)
  (declare (ignore colon-p at-sign-p))
  (check-types (integer-time date))
  (write-local-date date stream :format :yyyy-mm))


;; moved 'ddmmm365' to locations.lisp as 'loc%airimp-date'

(defun cl-user::loc%HH-MM (stream time colon-p at-sign-p)
  (declare (ignore colon-p at-sign-p))
  (write-local-tofd (get-local-time time) stream :format :hh-mm))

(defun cl-user::loc%_H-MM (stream time colon-p at-sign-p)
  (declare (ignore colon-p at-sign-p))
  (check-types (integer-time time))
  (write-local-tofd time stream :format :_h-mm))

(defun cl-user::loc%HHMM (stream time colon-p at-sign-p)
  (declare (ignore colon-p at-sign-p))
  (check-types (integer-time time))
  (write-local-tofd time stream :format :hhmm))

(defun cl-user::loc%HHMMAP (stream time colon-p at-sign-p)
  (declare (ignore colon-p at-sign-p))
  (check-types (integer-time time))
  (write-local-tofd time stream :format :hhmmap))

(defun cl-user::loc%_HMMAP (stream time colon-p at-sign-p)
  (declare (ignore colon-p at-sign-p))
  (check-types (integer-time time))
  (write-local-tofd time stream :format :_hmmap))

;; was HHMMAMPM
(defun cl-user::loc%HH-MM-AP (stream time colon-p at-sign-p)
  (declare (ignore colon-p at-sign-p))
  (check-types (integer-time time))
  (write-local-tofd time stream :format :hh-mm-ap))


(defun cl-user::loc%H-MM-AP-WORD (stream time colon-p at-sign-p)
  (declare (ignore colon-p at-sign-p))
  (check-types (integer-time time))
  (write-local-tofd time stream :format :h-mm-ap-word))


(defun cl-user::dur%H_M (stream duration colon-p at-sign-p)
  (declare (ignore colon-p at-sign-p))
  (check-types (integer duration))
  (let ((sign (cond ((< duration 0)
                     (setf duration (- duration))
                     "(-)")
                    (t ""))))
    (multiple-value-bind (s minutes hours)
        (decode-integer-tofd duration)
      (declare (ignore s))
      (if (zerop hours)
	(format stream "~A~Dm" sign minutes)
	(format stream "~A~Dh ~Dm" sign hours minutes)))))

(defun cl-user::dur%HHMM (stream duration colon-p at-sign-p)
  (declare (ignore colon-p at-sign-p))
  (check-types (integer duration))
  (let ((sign (cond ((< duration 0)
                     (setf duration (- duration))
                     "-")
                    (t ""))))
    (multiple-value-bind (s minutes hours)
        (decode-integer-tofd duration)
      (declare (ignore s))
      (if (zerop hours)
	(format stream "~A0:~2,'0D" sign minutes)
	(format stream "~A~D:~2,'0D" sign hours minutes)))))

(defun cl-user::dur%+HHMM (stream duration colon-p at-sign-p)
  (declare (ignore colon-p at-sign-p))
  (check-types (integer duration))
  (let ((sign (cond ((< duration 0)
                     (setf duration (- duration))
                     "-")
                    (t "+"))))
    (multiple-value-bind (s minutes hours)
        (decode-integer-tofd duration)
      (declare (ignore s))
      (if (zerop hours)
	(format stream "~A0:~2,'0D" sign minutes)
	(format stream "~A~D:~2,'0D" sign hours minutes)))))

(defun cl-user::dur%_hmm (stream duration colon-p at-sign-p)
  (declare (ignore colon-p at-sign-p))
  (check-types (integer duration))
  (let ((sign (cond ((< duration 0)
                     (setf duration (- duration))
                     "-")
                    (t ""))))
    (multiple-value-bind (s minutes hours)
        (decode-integer-tofd duration)
      (declare (ignore s))
      (if (zerop hours)
	(format stream "~A 0:~2,'0D" sign minutes)
	(format stream "~A~2,' D:~2,'0D" sign hours minutes)))))

;; "WKD1, MTH1 DD1"
;; "WKD1-WKD2, MTH1 DD1-DD2"
;; "WKD1-WKD2, MTH1 DD1-MTH2 DD2"
;; "MTH1 DD1, YYYY1"
;; "MTH1 DD1-MTH2 DD2, YYY1"
;; "MTH1 DD1, YYY1-MTH2 DD2, YYY2"
;;
;; case must be :UPPER or :MIXED.
;;
;; show-weekday must be NIL, :UPFRONT, :INTERSPERSED, or :AUTOMATIC.  If :AUTOMATIC, then acts like
;; :UPFRONT for ranges up to 7 days, and :INTERSPERSED for ranges of 8 days or more.
;;
(defun write-local-date-range (time1 time2 &optional (stream *standard-output*)
				     &key (case :upper) long-p (show-weekday :AUTOMATIC)
					  show-year-p)
  (unless (>= time2 time1)
    (rotatef time1 time2))

  (flet ((write-weekday (dofw)
	   (write-string (svref (if long-p
				    (ecase case
				      (:upper $uppercase-days-long)
                                        ;(:lower $lowercase-days-long)
				      (:mixed $mixedcase-days-long))
				    (ecase case
				      (:upper $uppercase-days)
                                        ;(:lower $lowercase-days)
				      (:mixed $mixedcase-days)))
				dofw)
			 stream))
	 (write-month (m)
	   (write-string (svref (if long-p
				    (ecase case
				      (:upper $uppercase-months-long)
                                        ;(:lower $lowercase-months-long)
				      (:mixed $mixedcase-months-long))
				    (ecase case
				      (:upper $uppercase-months)
                                        ;(:lower $lowercase-months)
				      (:mixed $mixedcase-months)))
				(i- m 1)) stream)))

    (declare (dynamic-extent #'write-weekday #'write-month))
    (multiple-value-bind (d1 m1 y1 dofw1)
	(decode-integer-date time1)
      (multiple-value-bind (d2 m2 y2 dofw2)
	  (decode-integer-date time2)
	(let* ((same-day-p (local-date-equal time1 time2))
	       (same-year-p (i= y1 y2))
	       (same-month-p (or same-day-p (and same-year-p (i= m1 m2)))))
	  (when (eq show-weekday :AUTOMATIC)
	    (setf show-weekday (if (<= (local-date-offset time1 time2) 7) :UPFRONT :INTERSPERSED)))
	  (when show-weekday
	    (write-weekday dofw1)
	    (unless same-day-p
	      (when (eq show-weekday :UPFRONT)
		(write-string "-" stream)
		(write-weekday dofw2)))
	    (write-string ", " stream))
	  (write-month m1)
	  (write-string " " stream)
	  (write-integer d1 stream)
	  (when (and show-year-p (or same-day-p (not same-year-p)))
	    (write-string ", " stream)
	    (write-integer y1 stream))
	  (unless same-day-p
	    (if (eq show-weekday :INTERSPERSED)
		(progn
		  (write-string " - " stream)
		  (write-weekday dofw2)
		  (write-string ", " stream))
		(write-string "-" stream))
	    (when (or (not same-month-p) (eq show-weekday :INTERSPERSED))
	      (write-month m2)
	      (write-string " " stream))
	    (write-integer d2 stream)
	    (when show-year-p
	      (write-string ", " stream)
	      (write-integer y2 stream))))))))


;; "WKD1, MTH1 DD1"
;; "WKD1-WKD2, MTH1 DD1-DD2"
;; "WKD1-WKD2, MTH1 DD1-MTH2 DD2"
;; 'time-pair' should be a cons of two integer-times
(defun cl-user::loc%WW-MTH-DD-PAIR (stream time-pair colon-p at-sign-p)
  (write-local-date-range (car time-pair) (cdr time-pair) stream
			  :case (if colon-p :mixed :upper) :long-p at-sign-p))

(defun cl-user::loc%WW-MTH-DD (stream date-local colon-p at-sign-p)
  (check-types (integer-date date-local))
  (let ((case (if colon-p :mixed :upper))
        (long-p at-sign-p))
    (flet ((write-weekday (dofw)
             (write-string (svref (if long-p
                                      (ecase case
                                        (:upper $uppercase-days-long)
                                        ;(:lower $lowercase-days-long)
                                        (:mixed $mixedcase-days-long))
                                      (ecase case
                                        (:upper $uppercase-days)
                                        ;(:lower $lowercase-days)
                                        (:mixed $mixedcase-days)))
                                  dofw)
                           stream))
           (write-month (m)
             (write-string (svref (if long-p
                                      (ecase case
                                        (:upper $uppercase-months-long)
                                        ;(:lower $lowercase-months-long)
                                        (:mixed $mixedcase-months-long))
                                      (ecase case
                                        (:upper $uppercase-months)
                                        ;(:lower $lowercase-months)
                                        (:mixed $mixedcase-months)))
                                  (i- m 1)) stream)))
      (declare (dynamic-extent #'write-weekday #'write-month))
      (multiple-value-bind (d m y dofw)
          (decode-integer-date date-local)
        (declare (ignore y))
        (write-weekday dofw)
        (write-string ", " stream)
        (write-month m)
        (write-string " " stream)
        (write-integer d stream)))))


(defun %show-all-new-time-formats (time)
  (cl-user::loc%DDMTH *standard-output* time nil nil) (write-char #\Newline)
  (cl-user::loc%DDMTHYY *standard-output* time nil nil) (write-char #\Newline)
  (cl-user::loc%DD-MTH-YYYY *standard-output* time nil nil) (write-char #\Newline)
  (cl-user::loc%YYYY-MM-DD-HH-MM-SS *standard-output* time nil nil) (write-char #\Newline)
  (cl-user::loc%MM-YY *standard-output* time nil nil) (write-char #\Newline)
  (cl-user::loc%MMYY *standard-output* time nil nil) (write-char #\Newline)
  (cl-user::loc%HHMM *standard-output* time nil nil) (write-char #\Newline)
  (cl-user::loc%HHMMAP *standard-output* time nil nil) (write-char #\Newline)
  (cl-user::loc%HH-MM-AP *standard-output* time nil nil) (write-char #\Newline)
  time
  )

(defun-inline itus-to-ms (itus)
  (round itus #.(/ internal-time-units-per-second 1000)))

(defun-inline itus-to-us (itus)
  (round itus #.(/ internal-time-units-per-second (expt 10 6))))
