;;;; format-errors.lisp

(in-package #:format-errors)

;;; "format-errors" goes here. Hacks and glory await!

;; internal definitions
(setf 3bmd-code-blocks:*code-blocks* t)

(defsection @format-errors-manual (:title "format-errors manual")
  "A small Common Lisp library to output value and errors in a way that
appeals to me and is used in the field I currently work in."
  (format-errors function)
  (*decimal-mark* variable)
  (*error-delimiters* variable)
  (*sum-error-delimiters* variable))



(defvar *base* 10
  "Internal use only as basis of log to calculate
error digits. If you change this, things will break")

(defvar *internal-separator-indicator* '-
  "Internal use only. Indicates seperator position in list of digits.")



;; definitions that can be rebound

(defvar *decimal-mark* #\.
  "Character used to indicate decimal mark in number. Defaults to '.'")

(defvar *error-delimiters* "()"
  "Delimiters used to indicate individual errors.")

(defvar *sum-error-delimiters* "{}"
  "Delimiters used to indicate square root of the sum of the squares of the errors.")


;;;; helper class used to keep things in order

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
  (if (= 0 value)
      -1
      (floor (log value *base*))))


(defun %get-sign (value)
  (if (= 0d0 value)
      1d0
      (/ value (abs value))))

(defun %split-digits (digits)
  (let ((split (split-sequence:split-sequence *internal-separator-indicator* digits)))
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
	 (collect *internal-separator-indicator* into digits)
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

(defun sqrt-sum (lst)
  (sqrt (reduce #'+ (mapcar #'(lambda (v) (* v v)) lst))))


(defun format-errors (value errors
		      &key (stream nil) (default-error-digits 2))
  "Given a value in VALUE and a simple list of errors in ERRORS,
FORMAT-ERRORS prints the value, list of errors, and the square root of
the sum of the squares of the errors in delimiters (as defined by
*ERROR-DELIMITERS* and *SUM-ERROR-DELIMITERS*) into STREAM (defaults
to nil) with DEFAUL-ERROR-DIGITS (defaults to 2) number of digits. 
Some examples:
```
(format-errors 3.1 '(0.21 0.03)) => 3.10(21)(03){21}
(format-errors 3.1 '(0.21)) => 3.10(21)
(format-errors 3.1 '(0.21 0.21)) => 3.10(21)(21){30}
(format-errors 3.1 '(0.9 0.9)) => 3.1(0.9)(0.9){1.3}
(format-errors 1.0012 '(0.0099 0.0079)) => 1.001(10)(08){13}
(format-errors 1.001 '(0.021 0.0003)) => 1.001(21)(00){21}
(format-errors 1.001 '(0.021 0.0003) :default-error-digits 3) => 1.0010(210)(003){210}
(format-errors 1.001 '(0.021 0.003)) => 1.001(21)(03){21}
(format-errors 30.15 '(3.1 0.3)) => 30.1(3.1)(0.3){3.1}
(format-errors 0 '(0.3 0.4)) => 0.00(30)(40){50}
(format-errors 30000.15 '(314 3001)) => 30000(310)(3000){3020}
(format-errors 30000.15 '(315 3001)) => 30000(320)(3000){3020}
(format-errors 30000.15 '(315 3051)) => 30000(320)(3050){3070}
```" 
  (labels ((conc-detail (detail &optional only-last-no-digits)
	     (with-slots (digits digits<one)
		 detail
	       (let+ ((has-separator (and
				      (find *internal-separator-indicator* digits)
				      only-last-no-digits
				      (< (length digits<one) only-last-no-digits)))
		      (digits-to-print
		       (if only-last-no-digits
			   (subseq digits (max 0 (- (length digits)
						    (+ only-last-no-digits
						       (if has-separator 1 0)))))
			   digits)))
		 (format nil "~{~a~}" (substitute *decimal-mark*
						  *internal-separator-indicator*
						  digits-to-print))))))
    (let+ ((sqrt-sum (sqrt-sum errors))
	   (max-error-magn (reduce #'max (append (list sqrt-sum) errors)
				   :key #'%get-order-of-magnitude))
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
				  errors))
	   (sum-sqr-detail (make-instance 'number-details
					  :value sqrt-sum
					  :down-to-order-of-magnitude down-to))
	   (no-error-digits (max (1+ max-error-magn)
				 default-error-digits)))
      (if (> (length errors) 1)
	  (format stream
	       (format nil "~~a~~{~a~~a~a~~}~a~~a~a"
		       (aref *error-delimiters* 0)
		       (aref *error-delimiters* 1)
		       (aref *sum-error-delimiters* 0)
		       (aref *sum-error-delimiters* 1))
	       (conc-detail value-detail)
	       (mapcar #'(lambda (d) (conc-detail d no-error-digits))
		       errors-detail)
	       (conc-detail sum-sqr-detail no-error-digits))
	  (format stream
	       (format nil "~~a~~{~a~~a~a~~}"
		       (aref *error-delimiters* 0)
		       (aref *error-delimiters* 1))
	       (conc-detail value-detail)
	       (mapcar #'(lambda (d) (conc-detail d no-error-digits))
		       errors-detail))))))





