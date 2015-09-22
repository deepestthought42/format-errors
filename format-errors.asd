;;;; format-errors.asd

(asdf:defsystem #:format-errors
  :description "Describe format-errors here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:iterate
               #:alexandria
		#:let-plus
		#:split-sequence)
  :serial t
  :components ((:file "package")
               (:file "format-errors")))

