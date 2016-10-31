
(defpackage :UTime
  (:use :CL)
  (:export #:current-nanoseconds-since-1900
           #:get-universal-time-ii ; integer universal-time, integer nanoseconds
           #:get-universal-time-ir ; integer universal-time, rational fraction of second
           #:get-universal-time-if ; integer universal-time, long-float fraction of second
           #:get-universal-time-f  ; long-float universal-time
           ))

(in-package :UTime)

(declaim (optimize speed (safety 1) (debug 0) (space 0)))

(defconstant precise-time-units-per-second (expt 10 9))

(defconstant universal-time-epoch-in-precise-time
  (loop for ut1 = (get-universal-time)
     for pt = (ccl:current-time-in-nanoseconds)
     for ut2 = (get-universal-time)
     until (= ut1 ut2)
     finally (return (* (- (truncate pt precise-time-units-per-second) ut1)
                        precise-time-units-per-second))))

(declaim (ftype (function () (unsigned-byte 64))
                current-nanoseconds-since-1900))

(declaim (inline current-nanoseconds-since-1900))

(defun current-nanoseconds-since-1900 ()
  (declare (optimize speed (safety 0) (debug 0)))
  (- (ccl:current-time-in-nanoseconds) universal-time-epoch-in-precise-time))

(declaim (ftype
          (function () (values
                        ;; ≅ (unsigned-byte 35)
                        (integer 0 #.(ceiling (/ (expt 2 64) precise-time-units-per-second)))
                        ;; ≅ (unsigned-byte 30)
                        (integer 0 (#.precise-time-units-per-second))))
          get-universal-time-ii))

;; First value same as CL:GET-UNIVERSAL-TIME, second value is
;; additional nanoseconds
(defun get-universal-time-ii ()
  (declare (optimize speed (safety 0) (debug 0)))
  (floor (current-nanoseconds-since-1900) precise-time-units-per-second))


(declaim (ftype
          (function () (values
                        ;; ≅ (unsigned-byte 35)
                        (integer 0 #.(ceiling (/ (expt 2 64) precise-time-units-per-second)))
                        rational))
          get-universal-time-ir))

;; First value same as CL:GET-UNIVERSAL-TIME, second value is fraction
;; of second as RATIONAL.
(defun get-universal-time-ir ()
  (declare (optimize speed (safety 0) (debug 0)))
  (multiple-value-bind (seconds fraction)
      (floor (current-nanoseconds-since-1900) precise-time-units-per-second)
    (values seconds
            (/ fraction precise-time-units-per-second))))


(declaim (ftype
          (function () (values
                        ;; ≅ (unsigned-byte 35)
                        (integer 0 #.(ceiling (/ (expt 2 64) precise-time-units-per-second)))
                        long-float))
          get-universal-time-if))

;; First value same as CL:GET-UNIVERSAL-TIME, second value is fraction
;; of second as LONG-FLOAT.
(defun get-universal-time-if ()
  (declare (optimize speed (safety 0) (debug 0)))
  (multiple-value-bind (seconds fraction)
      (floor (current-nanoseconds-since-1900) precise-time-units-per-second)
    (values seconds
            (/ (float fraction 1l0)
               #.(float precise-time-units-per-second 1l0)))))


(declaim (ftype (function () (long-float (0l0)))
                get-universal-time-f))

;; Universal time as a LONG-FLOAT with precision of either the
;; precision returned by CCL:CURRENT-TIME-IN-NANOSECONDS or the
;; precision allowed by a LONG-FLOAT, whichever is smaller.
;; 
;; On a system where LONG-FLOAT is IEEE-754 double and the timer has a
;; resolution of 100 ns and the epoch is a few centuries back
;; (e.g. CCL on Windows), in the the 21st century and beyond, the
;; limiting factor is the precision of the float. That is, all of the
;; digits of the float are significative and a little bit of
;; additional resolution is lost (about 2 bits, in 2016).
;; 
;; If maximum accuracy is desired use one of the other functions.
;; 
;; (multiple-value-bind (s ns) (get-universal-time-ii)
;;   (let ((utf (get-universal-time-f)))
;;     (format t "~D.~9,'0D~%" s ns)       ; Maximum resolution.
;;     (format t "~F~%" utf)))             ; A few bits are lost.

(defun get-universal-time-f ()
  (declare (optimize speed (safety 0) (debug 0)))
  (/ (float (current-nanoseconds-since-1900) 1l0)
     #.(float precise-time-units-per-second 1l0)))

;;; ====(Sun Oct 30 02:40:20 2016)==================================

#|

;; In CCL Windows X8664:
;; CCL:CURRENT-TIME-IN-NANOSECONDS allocates 32 bytes.
;; GET-UNIVERSAL-TIME-II allocates 96 bytes.
;; GET-UNIVERSAL-TIME-IR allocates 128 bytes.
;; GET-UNIVERSAL-TIME-IF allocates 128 bytes.
;; GET-UNIVERSAL-TIME-F allocates 96 bytes.

(defun test (&optional (seconds 3))
  (declare (optimize speed (safety 0)))
  (loop with start-time = (get-universal-time)
     with end-time = (+ start-time seconds)
     with diffs of-type fixnum = 0
     for counter of-type fixnum from 0
     for universal-time of-type integer = (get-universal-time)
     while (< universal-time end-time)
     do (multiple-value-bind (ut* nano) (get-universal-time-ii)
          (declare (type integer ut* nano))
          (unless (= universal-time ut*)
            (format t ";; diff: ~S ~S ~S~%" universal-time ut* nano)
            (incf diffs)
            (when (> diffs 10)
              (loop-finish))))
     finally (return (list counter diffs))))

(defun demo ()
  (loop for l in (list (multiple-value-list (get-universal-time))
                       (multiple-value-list (get-universal-time-ii))
                       (multiple-value-list (get-universal-time-ir))
                       (multiple-value-list (get-universal-time-if))
                       (multiple-value-list (get-universal-time-f)))
     do (print l)))

|#

;; "PETRVS·PAVLVS·NEPTVNENSIS·ME·FECIT·MMXVI"
