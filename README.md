
This file implements some variants of CL:GET-UNIVERSAL-TIME with
sub-second resolution for Clozure Common Lisp.

The times returned are based on whatever the function
CCL:CURRENT-TIME-IN-NANOSECONDS returns, that is, whatever the OS
returns. This means that currently leap seconds are ignored, the times
reported can jump back and forth at random, etc. All the usual stuff
which is also true for CL:GET-UNIVERSAL-TIME.

---

* current-nanoseconds-since-1900

  What the name says.

* get-universal-time-ii

  Returns two values, the first value is the same as
  cl:get-universal-time, the second value is a fraction of second
  expressed as an integer number of nanoseconds.

* get-universal-time-ir

  Returns two values, the first value is the same as
  cl:get-universal-time, the second value is a fraction of second
  expressed as a rational.

* get-universal-time-if

  Returns two values, the first value is the same as
  cl:get-universal-time, the second value is a fraction of second
  expressed as a long-float.

* get-universal-time-f

  Universal time as a long-float with precision of either the precision
  returned by ccl:current-time-in-nanoseconds or the precision allowed
  by a long-float, whichever is smaller.
  
  On a system where long-float is IEEE-754 double and the timer has a
  resolution of 100 ns and the epoch is a few centuries back (e.g. CCL
  on Windows), in the the 21st century and beyond, the limiting factor
  is the precision of the float. That is, all of the digits of the
  float are significative and a little bit of additional resolution is
  lost (about 2 bits, in 2016).

  If maximum accuracy is desired use one of the other functions.

    (multiple-value-bind (s ns) (get-universal-time-ii)
      (let ((utf (get-universal-time-f)))
        (format t "~D.~9,'0D~%" s ns)       ; Maximum resolution.
        (format t "~F~%" utf)))             ; A few bits are lost.
