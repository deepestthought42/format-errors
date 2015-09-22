;;;; format-errors.lisp

(in-package #:format-errors)

;;; "format-errors" goes here. Hacks and glory await!

(defparameter *base* 10)
(defparameter *separator* '-)


(defclass number-details ()
  ((value :accessor value :initarg :value
	  :initform (error "Need to supply value."))
   (digits :accessor digits :initarg :digits :initform '())
   (digits-down-to :accessor digits-down-to :initarg :digits-down-to :initform 0)
   (order-of-magnitude :accessor order-of-magnitude
		       :initarg :order-of-magnitude :initform 1d0)
   (sign :accessor sign :initarg :sign :initform 1d0)))


(defun %get-order-of-magnitude (value)
  (round (log value *base*)))

(defun %get-sign (value)
  (/ value (abs value)))

(defun %get-digits (value down-to)
  (let ((order-of-magnitude (%get-order-of-magnitude value)))
    (if (< order-of-magnitude 0)
	'()
	(iter
	  ;; rounding should take care of weird floating point effects
	  (with new-value = (* (round value (expt *base* down-to))
			       (expt *base* down-to)))
	  (for i from order-of-magnitude downto down-to)
	  (for digit = (floor (/ new-value (expt *base* i))))
	  (decf new-value (* digit (expt *base* i)))
	  (if (= -1 i)
	      (progn (collect *separator*) (collect digit))
	      (collect digit))))))




(defmethod initialize-instance :after ((obj number-details) &key)
  (with-slots (order-of-magnitude sign value)
      obj
    (setf order-of-magnitude (%get-order-of-magnitude value)
	  sign (%get-sign value))))



(defun format-errors (value errors &optional (default-no-digits 2))
  "")



#+nil
(prove:run #P"/home/renee/phd/src/lisp/format-errors/t/test.lisp")
