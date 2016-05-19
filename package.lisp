;;;; package.lisp

(mgl-pax:define-package #:format-errors
    (:documentation "A small Common Lisp library to output value and errors in a way that
appeals to me and is used in the field I currently work in.")
  (:use #:cl #:let-plus #:iterate #:mgl-pax)
  (:export
   #:format-errors
   #:*separator-character*))

