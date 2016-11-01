
(defpackage :UTime
  (:use :CL)
  (:export #:current-nanoseconds-since-1900
           #:get-universal-time-ii ; integer universal-time, integer nanoseconds
           #:get-universal-time-ir ; integer universal-time, rational fraction of second
           #:get-universal-time-if ; integer universal-time, long-float fraction of second
           #:get-universal-time-r  ; rational universal-time
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
  (multiple-value-bind (s ns)
      (floor (ccl:current-time-in-nanoseconds) precise-time-units-per-second)
    (values (- s #.(/ universal-time-epoch-in-precise-time precise-time-units-per-second))
            ns)))

#| 
;; Slower. Allocates more (96 bytes vs 80 bytes).
(defun get-universal-time-ii ()
  (declare (optimize speed (safety 0) (debug 0)))
  (floor (current-nanoseconds-since-1900) precise-time-units-per-second))
|#

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
      (floor (ccl:current-time-in-nanoseconds) precise-time-units-per-second)
    (values (- seconds #.(/ universal-time-epoch-in-precise-time precise-time-units-per-second))
            (/ fraction precise-time-units-per-second))))

#|
;; Slower. Allocates more (128 bytes vs 112 bytes)
(defun get-universal-time-ir ()
  (declare (optimize speed (safety 0) (debug 0)))
  (multiple-value-bind (seconds fraction)
      (floor (current-nanoseconds-since-1900) precise-time-units-per-second)
    (values seconds
            (/ fraction precise-time-units-per-second))))
|#

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
      (floor (ccl:current-time-in-nanoseconds) precise-time-units-per-second)
    (values (- seconds #.(/ universal-time-epoch-in-precise-time precise-time-units-per-second))
            (/ (float fraction 1l0)
               #.(float precise-time-units-per-second 1l0)))))

#|
Slower. Allocate more.

(defun get-universal-time-if ()
  (declare (optimize speed (safety 0) (debug 0)))
  (multiple-value-bind (seconds fraction)
      (floor (current-nanoseconds-since-1900) precise-time-units-per-second)
    (values seconds
            (/ (float fraction 1l0)
               #.(float precise-time-units-per-second 1l0)))))

(defun get-universal-time-if ()
  (declare (optimize speed (safety 0) (debug 0)))
  (multiple-value-bind (seconds fraction)
      (floor (ccl:current-time-in-nanoseconds) precise-time-units-per-second)
    (values (- seconds #.(/ universal-time-epoch-in-precise-time precise-time-units-per-second))
            (float (/ fraction
                      precise-time-units-per-second)
                   1l0))))
|#

(declaim (ftype (function () (rational (0)))
                get-universal-time-r))

(declaim (inline get-universal-time-r))

(defun get-universal-time-r ()
  (declare (optimize speed (safety 0) (debug 0)))
  (/ (current-nanoseconds-since-1900)
     precise-time-units-per-second))


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
;; additional resolution is lost (about 2 bits, in 2016):
;; 
;; (- (log (/ (current-nanoseconds-since-1900) 100) 2l0)
;;    (float-digits 1l0))
;; 2.0332794619791557D0
;; 
;; If maximum accuracy is desired use one of the other functions.
;; 
;; (multiple-value-bind (s ns) (get-universal-time-ii)
;;   (let ((utf (get-universal-time-f)))
;;     (format t "~D.~9,'0D~%" s ns)       ; Maximum resolution.
;;     (format t "~F~%" utf)))             ; A few bits are lost.

(defun get-universal-time-f ()
  (declare (optimize speed (safety 0) (debug 0) (space 0)))
  (float (get-universal-time-r) 1l0))

;;; ====(Sun Oct 30 02:40:20 2016)==================================

#|

;; In CCL Windows X8664:
;; CCL:CURRENT-TIME-IN-NANOSECONDS allocates 32 bytes.
;; CURRENT-NANOSECONDS-SINCE-1900 allocates 32 bytes.
;; GET-UNIVERSAL-TIME-II allocates 80 bytes.
;; GET-UNIVERSAL-TIME-IR allocates 112 bytes.
;; GET-UNIVERSAL-TIME-IF allocates 112 bytes.
;; GET-UNIVERSAL-TIME-R allocates 32 bytes.
;; GET-UNIVERSAL-TIME-F allocates 176 bytes.

(defun demo ()
  (loop for fun in '(get-universal-time get-universal-time-ii get-universal-time-ir
                     get-universal-time-if get-universal-time-r get-universal-time-f)
     collect (list fun (multiple-value-list (funcall fun)))))

|#

;; "PETRVS·PAVLVS·NEPTVNENSIS·ME·FECIT·MMXVI"
