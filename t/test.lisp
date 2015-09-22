(in-package :cl-user)

(defpackage format-errors-test
  (:use :cl
	:format-errors
        :prove))

(in-package :format-errors-test)

(plan 3)

(is (format-errors 3.1 '(0.21 0.03)) "3.10(21)(03)")
(is (format-errors 1.001 '(0.021 0.003)) "1.001(21)(03)")
(is (format-errors 30.15 '(3.1 0.3)) "30.1(3.1)(0.3)")

(finalize)
