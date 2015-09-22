;;;; format-errors.lisp

(in-package #:format-errors)

;;; "format-errors" goes here. Hacks and glory await!

(defvar *base* 10)
(defvar *separator-indicator* '-)
(defvar *separator-character* #\.)

(defclass number-details ()
  ((value :accessor value :initarg :value
	  :initform (error "Need to supply value."))
   (down-to-order-of-magnitude :accessor down-to-order-of-magnitude
			       :initarg :down-to-order-of-magnitude
			       :initform (error "Need to supply down-to-order-of-magnitude"))
   (digits :accessor digits)
   (digits>one :accessor digits>one)
   (digits<one :accessor digits<one)
   (order-of-magnitude :accessor order-of-magnitude)
   (sign :accessor sign)))


(defun %get-order-of-magnitude (value)
  (floor (log value *base*)))

(defun %get-sign (value)
  (/ value (abs value)))

(defun %split-digits (digits)
  (let ((split (split-sequence:split-sequence *separator-indicator* digits)))
    (values (first split) (second split))))

(defun %get-digits (value down-to)
  (let+ ((order-of-magnitude (%get-order-of-magnitude value))
	 (from (max 0 order-of-magnitude)))
    (iter
      ;; rounding should take care of weird floating point effects
      (with new-value = (* (round value (expt *base* down-to))
			   (expt *base* down-to)))
      (for i from from downto (min 0 down-to))
      (for digit = (floor (/ new-value (expt *base* i))))
      (decf new-value (* digit (expt *base* i)))
      (cond
	((< i down-to)
	 (collect 0 into digits))
	((= -1 i)
	 (collect *separator-indicator* into digits)
	 (collect digit into digits))
	(t
	 (collect digit into digits)))
      (finally
       (return (values digits from down-to))))))





(defmethod initialize-instance :after ((obj number-details) &key)
  (with-slots (order-of-magnitude sign digits digits>one digits<one
	       value down-to-order-of-magnitude)
      obj
    (let+ ((.sign (%get-sign value))
	   (.value (abs (coerce value 'double-float)))
	   (.digits (%get-digits .value down-to-order-of-magnitude))
	   ((&values >one <one) (%split-digits .digits)))
      (setf order-of-magnitude (%get-order-of-magnitude .value)
	    sign .sign
	    digits .digits
	    digits<one <one
	    digits>one >one))))



(defun format-errors (value errors &key (stream nil) (default-error-digits 2))
  (labels ((conc-detail (detail &optional only-last-no-digits)
	     (with-slots (digits digits<one)
		 detail
	       (let+ ((has-separator (and
				      (find *separator-indicator* digits)
				      only-last-no-digits
				      (< (length digits<one) only-last-no-digits)))
		      (digits-to-print
		       (if only-last-no-digits
			   (subseq digits (max 0 (- (length digits)
						    (+ only-last-no-digits
						       (if has-separator 1 0)))))
			   digits)))
		 (format nil "~{~a~}" (substitute *separator-character*
						  *separator-indicator*
						  digits-to-print))))))
    (let+ ((max-error-magn (reduce #'max errors :key #'%get-order-of-magnitude))
	   (.down-to (- max-error-magn default-error-digits))
	   (down-to (if (<= .down-to 0)
			(1+ .down-to) .down-to))
	   (value-detail (make-instance 'number-details
					:value value
					:down-to-order-of-magnitude down-to))
	   (errors-detail (mapcar #'(lambda (e)
				      (make-instance 'number-details
						     :value e
						     :down-to-order-of-magnitude down-to))
				  errors)))
      (format stream
	      "~a~{(~a)~}"
	      (conc-detail value-detail)
	      (mapcar #'(lambda (d) (conc-detail d (max (1+ max-error-magn)
						   default-error-digits)))
		      errors-detail)))))





#+nil
(prove:run #P"/home/renee/phd/src/lisp/format-errors/t/test.lisp")
