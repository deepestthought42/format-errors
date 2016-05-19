;;;; format-errors.asd

(asdf:defsystem #:format-errors
  :description "format-errors is a library to output values and their
  errors in the way that is standard in the field I'm working
  in (physics). Example: 1.2(0.4)(0.3){0.5}"
  :author "Renee Klawitter <deepestthought42_at_gmail.com>"
  :version "1.0.0"
  :license "MIT license (see README.org)"
  :depends-on (#:iterate
	       #:alexandria
	       #:let-plus
		#:split-sequence
		#:mgl-pax)
  :serial t
  :components ((:file "package")
               (:file "format-errors")))

