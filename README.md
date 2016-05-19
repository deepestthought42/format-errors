# format-errors manual

###### \[in package FORMAT-ERRORS\]
A small Common Lisp library to output value and errors in a way that
appeals to me and is used in the field I currently work in.

- [function] FORMAT-ERRORS VALUE ERRORS &KEY (STREAM NIL) (DEFAULT-ERROR-DIGITS 2)

    Given a value in VALUE and a simple list of errors in ERRORS,
    FORMAT-ERRORS prints the value, list of errors, and the square root of
    the sum of the squares of the errors in delimiters (as defined by
    *ERROR-DELIMITERS* and *SUB-ERROR-DELIMITERS*) into STREAM (defaults
    to nil) with DEFAUL-ERROR-DIGITS (defaults to 2) number of digits. 
    Some examples:
    `
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
    `

- [variable] *BASE* 10

    Internal use only as basis of log to calculate
    error digits. If you change this, things will break

- [variable] *INTERNAL-SEPARATOR-INDICATOR* -

    Internal use only. Indicates seperator position in list of digits.
