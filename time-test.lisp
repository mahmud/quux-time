;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                  ;;;
;;; Confidential and proprietary information of ITA Software, Inc.   ;;;
;;;                                                                  ;;;
;;; Copyright (c) 2006 ITA Software, Inc.  All rights reserved.      ;;;
;;;								     ;;;
;;; Original author: Adam Worrall                                    ;;;
;;;								     ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This module provides xUnit tests for rigged & cached clocks

(in-package :quux-test)

(defun showtime (time &optional (message ""))
  (format t "~A: ~A (" message time)
  (cl-user::loc%YYYY-MM-DD-HH-MM-SS *standard-output* time nil nil)
  (format t ")~%"))

(defun wait-until-midsecond ()
  "Busyloop until the fractional seconds of the system clock are inside (0.4, 0.6)."
  (loop while t do
        (let* ((rt-now  (get-internal-real-time))             ;; e.g. 4431428
               (rt-base-sec (* internal-time-units-per-second ;; e.g. 4431000
                               (truncate (/ rt-now internal-time-units-per-second))))
               (fractional-second (/ (- rt-now rt-base-sec)   ;; e.g      428/1000
                                     internal-time-units-per-second))
               )
          (process-sleep 0.02)
          (when (and (> fractional-second 0.4)
                     (< fractional-second 0.6))
            (return)))))

(define-test-function time-flows-normally (seconds message)
  "Check that if we snooze for SECONDS seconds, then current-time-utc increases
by SECONDS seconds."
  ;; Consider what the real time T could be as this executes ...
  ;; T =  8.99s
  ;;              t-start := current_time_utc     (e.g. := 8)
  ;; T =  9.01s
  ;;              process-sleep 1
  ;; T = 10.01s
  ;;              t-end   := current_time_utc     (e.g. := 10)
  ;;
  ;; So, it's possible that the time diff will be 2, and not 1. Bah!
  ;; (The diff could even be 0, if process-sleep is under a second, and T=9.01 when t_start is
  ;;  measured ...)
  ;;
  ;; Bodge fix: use the real, underlying timer to busy-loop until we're approximately
  ;;  halfway between seconds. Then we can be pretty sure that the two measurements will
  ;;  occur in separate seconds.
  ;; To avoid those embarrassing test failure messages, make the test <=
  (wait-until-midsecond)
  (let ((t-start (current-time-utc)))
    (process-sleep seconds)
    (let ((t-end (current-time-utc)))
      (assert-equal seconds (- t-end t-start) :message message
                                              :test #'<=))))

(defmacro define-time-test (test args &body body)
  `(define-test ,test ,args
    (without-current-time-trickery ,@body)))

(define-time-test test-normal-time ()
  "Checks the normal behavior: no caching, no skewing of time."
  (let ((seconds 1))
    (call-test-function time-flows-normally seconds "normal-ticks")))

(defparameter $dur-snooze 1 "Process-Sleep for one wallclock second")

(define-time-test test-cached-time ()
  "Tests freezing (and thawing) of current time using the macros
with/without-current-time-cached."

  (let ((t-start (current-time-utc)))

    ;; freeze the current time (at t-start)
    (with-current-time-cached ()

      ;; sleep a second
      (process-sleep $dur-snooze)

      ;; frozen time hasn't changed
      (assert-equal (current-time-utc) t-start)

      ;; Check that the time can be unfrozen in-scope ...
      (without-current-time-cached ()
        (call-test-function time-flows-normally $dur-snooze
			    "without-current-time-cached"))

      ;; ... and that things continue afterwards
      (let ((t-start (current-time-utc)))
	(process-sleep $dur-snooze)
	(assert-equal (current-time-utc) t-start)))

    ;; freeze current time at fake-time
    (let ((fake-time 17))
      (with-current-time-cached (fake-time)
        (assert-equal (current-time-utc) fake-time :message "faked time before")
        (process-sleep $dur-snooze)
        (assert-equal (current-time-utc) fake-time :message "faked time after")))

    ;; Check things are back to normal
    (call-test-function time-flows-normally $dur-snooze "outside of cached scope")))

(define-time-test test-accelerated-time ()
  "Tests acceleration of (fake) time using the time-speeder-upper time function."
  (let ((speedup-rate 67))
    (with-current-time-function ((time-speeder-upper speedup-rate))
      (let ((t-start (current-time-utc)))
        (process-sleep $dur-snooze)
        (let ((t-end (current-time-utc)))
          (assert-equal (- t-end t-start) (* $dur-snooze speedup-rate)
			:message "time fast-forward"
                        :test #'>=))))

    ;; Check things are back to normal
    (call-test-function time-flows-normally $dur-snooze
			"outside of rigged scope")))

(define-time-test test-cached-accelerated-time ()
  "Tests combination of accelerated time and cached time."
  (let ((speedup-rate 67))
    ;; first speed up
    (with-current-time-function ((time-speeder-upper speedup-rate))
      (let ((t-start (current-time-utc)))
        (process-sleep $dur-snooze)
        (let ((t-pre-cache (current-time-utc)))

	  ;; then cache
          (with-current-time-cached ()
            (process-sleep $dur-snooze)
            (let ((t-mid-cache (current-time-utc)))
              (assert-equal t-pre-cache t-mid-cache
			    :message "cache works inside rigging")))

	  ;; go back to accelerated time
          (process-sleep $dur-snooze)
          (let ((t-end (current-time-utc)))
            (assert-equal (* $dur-snooze speedup-rate 3) (- t-end t-start)
			  :message "rigging restored"
                          :test #'<=)))))))

(define-time-test test-nested-accelerated-time ()
  "Tests nesting of accelerated time."
  (let ((outer-rate 7)
        (inner-rate 67))

    ;; This nested call should trigger a SIMPLE-ERROR condition
    (assert-error simple-error
      (with-current-time-function ((time-speeder-upper 3))
	(with-current-time-function ((time-speeder-upper 7)))))

    ;; This nested call should work.
    (let ((t-start (current-time-utc)))
      (with-current-time-function ((time-speeder-upper outer-rate))
        (with-current-time-function
	    ((time-speeder-upper inner-rate) :allow-nesting? t)
	  (process-sleep $dur-snooze)
	  (let ((t-end (current-time-utc)))
	    (assert-equal (- t-end t-start)
			  (* $dur-snooze inner-rate outer-rate)
			  :message "nested rigs correct"
                          :test #'>=)))))))

(define-time-test test-accelerated-cached-time ()
  "Checks that the outer scope caching the time wins over an inner acceleration."
  (let ((cached-time 86400)   ; 2nd Jan, 1900
        (rate 7))

    ;; freeze the time
    (with-current-time-cached (cached-time)

      ;; This nested call should trigger a SIMPLE-ERROR condition
      (assert-error simple-error
	(with-current-time-function ((time-speeder-upper rate))))

      ;; This nested call should be OK
      (with-current-time-function ((time-speeder-upper rate) :allow-nesting? t)
        (let ((t-start (current-time-utc)))
          (assert-equal cached-time t-start)
	  (process-sleep $dur-snooze)
	  (assert-equal (current-time-utc) t-start))))))

(define-time-test test-accelerated-time-increments ()
  "Ensure sped-up time has finer increments than one wallclock second."
  (with-current-time-function ((time-speeder-upper-hires 1000))
    (let ((t-start (current-time-utc)))
      (process-sleep 0.1)
      (let ((t-end (current-time-utc)))
	(assert-not-equal t-start t-end)))))

(define-test test-roll-time ()
  "Test the ROLL-TIME function"

  (flet ((ymd (year month &optional (day 1))
           (encode-integer-time 0 0 0 day month year)))
    (assert-equal (ymd 2006 1 1)
                  (roll-time (ymd 2005 1 1) :years 1))
    (assert-equal (ymd 2006 2 28)
                  (roll-time (ymd 2006 3 31) :months -1))
    (assert-equal (ymd 2006 2 28)
                  (roll-time (ymd 2006 3 30) :months -1))
    (assert-equal (ymd 2006 2 28)
                  (roll-time (ymd 2006 3 29) :months -1))
    (assert-equal (ymd 2006 2 28)
                  (roll-time (ymd 2006 3 28) :months -1))
    (assert-equal (ymd 2006 2 27)
                  (roll-time (ymd 2006 3 27) :months -1))
    (assert-equal (ymd 2000 1 1)
                  (roll-time (ymd 1996 1 1) :years 4)))
  
  
  (dotimes (i 100)
    (let* ((direction (case (random 2)
                        (0 +1)
                        (1 -1)))
           (second (random 60))
           (second-delta (* direction (random 1000)))
           (minute (random 60))
           (minute-delta (* direction (random 1000)))
           (hour (random 24))
           (hour-delta (* direction (random 1000)))
           (year (+ 2000 (random 100)))
           (year-delta (* direction (random 100)))
           (month (1+ (random 12)))
           ;; Avoid problematical February 29
           (day (1+ (random (days-per-month month 2007))))
           (day-delta (* direction (random 1000)))
           (start-time (encode-integer-time second minute hour day month year)))
      (assert-equal start-time
                    (roll-time
                     (roll-time start-time
                                :seconds second-delta
                                :minutes minute-delta
                                :hours hour-delta)
                     :seconds (- second-delta)
                     :minutes (- minute-delta)
                     :hours (- hour-delta))
                    :message (format nil "Failed to roll (encode-integer-time ~D ~D ~D ~D ~D ~D) by ~D ~D ~D 0 0 0"
                                     second minute hour day month year
                                     second-delta minute-delta hour-delta))
      (assert-equal start-time
                    (roll-time (roll-time start-time :days day-delta) :days (- day-delta))
                    :message (format nil "Failed to roll (encode-integer-time ~D ~D ~D ~D ~D ~D) by ~D days "
                                     second minute hour day month year day-delta))
      (assert-equal start-time
                    (roll-time (roll-time start-time :years year-delta) :years (- year-delta))
                    :message (format nil "Failed to roll (encode-integer-time ~D ~D ~D ~D ~D ~D) by ~D years "
                                     second minute hour day month year year-delta)))))


(define-test test-parse-local-time ()
  (debugf "test-parse-local-time current-time-utc: ~S" (current-time-utc))
  (assert-equal (encode-integer-time 1 2 3 4 5 2006)
                (parse-local-time "2006-05-04 03:02:01"))
  (assert-equal (encode-integer-time 1 2 3 4 5 2006)
                (parse-local-time "junk2006-05-04 03:02:01morejunk" :start 4 :end 23))
  (assert-equal (encode-integer-time 1 2 3 4 5 2006)
                (parse-local-time "junk2006-05-04 03:02:01" :start 4))
  (assert-equal (encode-integer-time 1 2 3 4 5 2006)
                (parse-local-time "2006-05-04 03:02:01junk" :end 19))
  (with-current-time-cached ((encode-integer-time 0 0 0 3 5 2006))
    (debugf "test-parse-local-time current-time-utc: ~S" (current-time-utc))
    (assert-equal (encode-integer-time 1 2 3 4 5 1983)
                  (parse-local-time "MAY4 03:02:01" :guess-year 1983))
    (assert-equal (encode-integer-time 1 2 3 4 5 1983)
                  (parse-local-time "4MAY 03:02:01" :guess-year 1983))
    (assert-equal (encode-integer-time 1 2 3 4 5 2006)
                  (parse-local-time "MAY4 03:02:01" :guess-year :utc-future))
    (assert-equal (encode-integer-time 1 2 3 4 5 2006)
                  (parse-local-time "4MAY 03:02:01" :guess-year :utc-future))
    (assert-equal (encode-integer-time 1 2 3 4 5 2006)
                  (parse-local-time "MAY4 03:02:01" :guess-year :utc-current))
    (assert-equal (encode-integer-time 1 2 3 4 5 2006)
                  (parse-local-time "4MAY 03:02:01" :guess-year :utc-current))
    (assert-equal (encode-integer-time 1 2 3 4 5 2005)
                  (parse-local-time "MAY4 03:02:01" :guess-year :utc-past))
    (assert-equal (encode-integer-time 1 2 3 4 5 2005)
                  (parse-local-time "4MAY 03:02:01" :guess-year :utc-past))
    (assert-equal nil
                  (parse-local-time "MAY4 03:02:01" :guess-year nil))
    (assert-equal nil
                  (parse-local-time "4MAY 03:02:01" :guess-year nil))
    (assert-equal (encode-integer-time 1 2 3 4 5 2006)
                  (parse-local-time "04MAY06 03:02:01" :guess-century :utc-nearest))
    (assert-equal (encode-integer-time 1 2 3 4 5 1906)
                  (parse-local-time "04MAY06 03:02:01" :guess-century :utc-past))
    (assert-equal (encode-integer-time 1 2 3 4 5 2006)
                  (parse-local-time "04MAY06 03:02:01" :guess-century :utc-future)))
  (with-current-time-cached ((encode-integer-time 0 0 0 4 5 2006))
    (debugf "test-parse-local-time current-time-utc: ~S" (current-time-utc))
    (assert-equal (encode-integer-time 1 2 3 4 5 1983)
                  (parse-local-time "MAY4 03:02:01" :guess-year 1983))
    (assert-equal (encode-integer-time 1 2 3 4 5 1983)
                  (parse-local-time "4MAY 03:02:01" :guess-year 1983))
    (assert-equal (encode-integer-time 1 2 3 4 5 2006)
                  (parse-local-time "MAY4 03:02:01" :guess-year :utc-future))
    (assert-equal (encode-integer-time 1 2 3 4 5 2006)
                  (parse-local-time "4MAY 03:02:01" :guess-year :utc-future))
    (assert-equal (encode-integer-time 1 2 3 4 5 2006)
                  (parse-local-time "MAY4 03:02:01" :guess-year :utc-current))
    (assert-equal (encode-integer-time 1 2 3 4 5 2006)
                  (parse-local-time "4MAY 03:02:01" :guess-year :utc-current))
    (assert-equal (encode-integer-time 1 2 3 4 5 2006)
                  (parse-local-time "MAY4 03:02:01" :guess-year :utc-past))
    (assert-equal (encode-integer-time 1 2 3 4 5 2006)
                  (parse-local-time "4MAY 03:02:01" :guess-year :utc-past))
    (assert-equal nil
                  (parse-local-time "MAY4 03:02:01" :guess-year nil))
    (assert-equal nil
                  (parse-local-time "4MAY 03:02:01" :guess-year nil))
    (assert-equal (encode-integer-time 1 2 3 4 5 2006)
                  (parse-local-time "04MAY06 03:02:01" :guess-century :utc-nearest))
    (assert-equal (encode-integer-time 1 2 3 4 5 1906)
                  (parse-local-time "04MAY06 03:02:01" :guess-century :utc-past))
    (assert-equal (encode-integer-time 1 2 3 4 5 2006)
                  (parse-local-time "04MAY06 03:02:01" :guess-century :utc-future)))

  (with-current-time-cached ((encode-integer-time 0 0 0 5 5 2006))
    (debugf "test-parse-local-time current-time-utc: ~S" (current-time-utc))
    (assert-equal (encode-integer-time 1 2 3 4 5 1983)
                  (parse-local-time "MAY4 03:02:01" :guess-year 1983))
    (assert-equal (encode-integer-time 1 2 3 4 5 1983)
                  (parse-local-time "4MAY 03:02:01" :guess-year 1983))
    (assert-equal (encode-integer-time 1 2 3 4 5 2007)
                  (parse-local-time "MAY4 03:02:01" :guess-year :utc-future))
    (assert-equal (encode-integer-time 1 2 3 4 5 2007)
                  (parse-local-time "4MAY 03:02:01" :guess-year :utc-future))
    (assert-equal (encode-integer-time 1 2 3 4 5 2006)
                  (parse-local-time "MAY4 03:02:01" :guess-year :utc-current))
    (assert-equal (encode-integer-time 1 2 3 4 5 2006)
                  (parse-local-time "4MAY 03:02:01" :guess-year :utc-current))
    (assert-equal (encode-integer-time 1 2 3 4 5 2006)
                  (parse-local-time "MAY4 03:02:01" :guess-year :utc-past))
    (assert-equal (encode-integer-time 1 2 3 4 5 2006)
                  (parse-local-time "4MAY 03:02:01" :guess-year :utc-past))
    (assert-equal nil
                  (parse-local-time "MAY4 03:02:01" :guess-year nil))
    (assert-equal nil
                  (parse-local-time "4MAY 03:02:01" :guess-year nil))
    (assert-equal (encode-integer-time 1 2 3 4 5 2006)
                  (parse-local-time "04MAY06 03:02:01" :guess-century :utc-nearest))
    (assert-equal (encode-integer-time 1 2 3 4 5 2006)
                  (parse-local-time "04MAY06 03:02:01" :guess-century :utc-past))
    (assert-equal (encode-integer-time 1 2 3 4 5 2106)
                  (parse-local-time "04MAY06 03:02:01" :guess-century :utc-future)))
  (debugf "test-parse-local-time current-time-utc: ~S" (current-time-utc))
  (assert-equal (encode-integer-time 1 2 3 4 5 1970)
                (parse-local-time "04MAY70 03:02:01" :guess-century :unix-y2k))
  (assert-equal (encode-integer-time 1 2 3 4 5 2069)
                (parse-local-time "04MAY69 03:02:01" :guess-century :unix-y2k))
  ;; non-existent date
  (assert-equal nil
                (parse-local-time "2006-02-29 03:02:01"))
  (assert-equal nil
                (parse-local-time "2006-06-31 03:02:01"))
  ;; there was a bug having to do with 12am and 12pm
  (assert-equal (encode-integer-time 0 0 0 4 5 2006)
                (parse-local-time "2006-05-04 00:00:00"))
  (assert-equal (encode-integer-time 0 0 12 4 5 2006)
                (parse-local-time "2006-05-04 12:00:00"))
  (assert-equal (encode-integer-time 0 0 0 5 5 2006)
                (parse-local-time "2006-05-04 24:00:00"))
  (assert-equal (encode-integer-time 0 0 0 4 5 2006)
                (parse-local-time "2006-05-04 12:00:00AM"))
  (assert-equal (encode-integer-time 1 0 0 4 5 2006)
                (parse-local-time "2006-05-04 12:00:01AM"))
  (assert-equal (encode-integer-time 0 59 0 4 5 2006)
                (parse-local-time "2006-05-04 12:59AM"))
  (assert-equal (encode-integer-time 0 0 1 4 5 2006)
                (parse-local-time "2006-05-04 1:00AM"))
  (assert-equal (encode-integer-time 0 0 12 4 5 2006)
                (parse-local-time "2006-05-04 12:00:00PM"))
  (assert-equal (encode-integer-time 1 0 12 4 5 2006)
                (parse-local-time "2006-05-04 12:00:01PM"))
  (assert-equal (encode-integer-time 0 59 12 4 5 2006)
                (parse-local-time "2006-05-04 12:59PM"))
  (assert-equal (encode-integer-time 0 0 13 4 5 2006)
                (parse-local-time "2006-05-04 1:00PM"))
  ;; various formats
  (assert-equal (encode-integer-time 1 2 3 4 5 2006)
                (parse-local-time "4MAY2006 030201"))
  (assert-equal (encode-integer-time 1 2 3 4 5 2006)
                (parse-local-time "4MAY06T030201"))
  (assert-equal (encode-integer-time 1 2 3 4 5 2006)
                (parse-local-time "4-MAY-06 030201"))
  (assert-equal (encode-integer-time 1 2 3 4 5 2006)
                (parse-local-time "04052006T030201"))
  (assert-equal (encode-integer-time 1 2 3 4 5 2006)
                (parse-local-time "04-05-2006 03:02:01"))
  (assert-equal (encode-integer-time 1 2 3 4 5 2006)
                (parse-local-time "2006MAY4 3:02:01"))
  (assert-equal (encode-integer-time 0 2 3 4 5 2006)
                (parse-local-time "4-5-2006 0302"))
  (assert-equal (encode-integer-time 0 2 15 31 12 2006)
                (parse-local-time "31-12-2006 0302pm"))
  (assert-equal (encode-integer-time 1 2 3 4 5 2006)
                (parse-local-time "4/5/2006 03:02:01"))
  (assert-equal (encode-integer-time 1 2 3 4 5 2006)
                (parse-local-time "04/05/2006 03:02:01"))
  (assert-equal (encode-integer-time 1 2 3 4 5 2006)
                (parse-local-time "2006/5/4 03:02:01"))
  (assert-equal (encode-integer-time 1 2 3 4 5 2006)
                (parse-local-time "2006/05/04 03:02:01"))
  ;; unparsable date
  (assert-equal nil
                (parse-local-time "452006 03:02:01"))
  ;; unparsable time
  (assert-equal nil
                (parse-local-time "2006-05-04 03pm"))
  (assert-equal nil
                (parse-local-time "2006-05-04 302"))
  (assert-equal nil
                (parse-local-time "2006-05-04 03.02.01")))
  

;; These tests are a tad brittle (see 
(define-test-suite test-time ()
    (test-normal-time
     test-cached-time
     test-accelerated-time
     test-cached-accelerated-time
     test-nested-accelerated-time
     test-accelerated-cached-time
     test-accelerated-time-increments
     test-roll-time
     test-parse-local-time))

(register-test 'test-time)
