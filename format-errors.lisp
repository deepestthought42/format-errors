;;;; format-errors.lisp

(in-package #:format-errors)

;;; "format-errors" goes here. Hacks and glory await!



(defclass number-details ()
  ((value :accessor value :initarg :value
	  :initform (error "Need to supply value."))
   (digits<1 :accessor digits<1 :initarg :digits<1 :initform '())
   (digits>1 :accessor digits>1 :initarg :digits>1 :initform '())
   (order-of-magnitude :accessor order-of-magnitude
		       :initarg :order-of-magnitude :initform 1d0)
   (sign :accessor sign :initarg :sign :initform 1d0)))


(defun %get-digits>1 (value order-of-magnitude)
  (iter
    (for i from 0 to order-of-magnitude)
    (collect (round (/ value (expt 10 i))))))

(defmethod initialize-instance :after ((obj number-details) &key)
  (with-slots (order-of-magnitude sign value)
      obj
    (setf order-of-magnitude (round (log value 10d0))
	  sign (/ value (abs value)))))



(defun format-errors (value errors &optional (default-no-digits 2))
  "")



#+nil
(prove:run #P"/home/renee/phd/src/lisp/format-errors/t/test.lisp")
