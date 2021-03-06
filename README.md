
This code implements some variants of CL:GET-UNIVERSAL-TIME with
sub-second resolution for Clozure Common Lisp.

The times returned are based on whatever the function
CCL:CURRENT-TIME-IN-NANOSECONDS returns, that is, whatever the OS
returns. This means that currently leap seconds are ignored, the times
reported can jump back and forth at random, etc. All the usual stuff
which is also true for CL:GET-UNIVERSAL-TIME.

---

* high-resolution-time-units-per-second
* (get-high-resolution-real-time)
  
  Like CL:INTERNAL-TIME-UNITS-PER-SECOND and
  CL:GET-INTERNAL-REAL-TIME, but with higher resolution.

* (current-nanoseconds-since-1900)

  What the name says.

* (get-universal-time-ii)

  Returns two values, the first value is the same as
  CL:GET-UNIVERSAL-TIME, the second value is a fraction of second
  expressed as an integer number of high resolution time units.

* (get-universal-time-ir)

  Returns two values, the first value is the same as
  CL:GET-UNIVERSAL-TIME, the second value is a fraction of second
  expressed as a rational.

* (get-universal-time-if)

  Returns two values, the first value is the same as
  CL:GET-UNIVERSAL-TIME, the second value is a fraction of second
  expressed as a long-float.

* (get-universal-time-r)

  Universal-time as a rational.

* (get-universal-time-f)

  Universal time as a long-float with precision of either the precision
  returned by CCL:CURRENT-TIME-IN-NANOSECONDS or the precision allowed
  by a LONG-FLOAT, whichever is smaller.
  
  On a system where LONG-FLOAT is IEEE-754 double and the timer has a
  resolution of 100 ns and the epoch is a few centuries back (e.g. CCL
  on Windows), in the the 21st century and beyond, the limiting factor
  is the precision of the float. That is, all of the digits of the
  float are significative and a little bit of additional resolution is
  lost (about 2 bits, in 2016):
  
      (- (log (/ (current-nanoseconds-since-1900) 100) 2l0)
         (float-digits 1l0))
      2.0332794619791557D0
  
  If maximum accuracy is desired use one of the other functions.
  
      (multiple-value-bind (s ns) (get-universal-time-ii)
        (let ((utf (get-universal-time-f)))
          (format t "~D.~9,'0D~%" s ns)       ; Maximum resolution.
          (format t "~F~%" utf)))             ; A few bits are lost.
