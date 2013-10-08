(defpackage :hunchentoot-single-signon
  (:use :cl)
  (:documentation "Implementation of SPNEGO authentication for Hunchentoot")
  (:export #:spnego-auth))

(in-package :hunchentoot-single-signon)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *compile-decl* '(optimize (speed 0) (safety 3) (debug 3))))
