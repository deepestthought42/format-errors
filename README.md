<a id='x-28FORMAT-ERRORS-3A-40FORMAT-ERRORS-MANUAL-20MGL-PAX-3ASECTION-29'></a>

# format-errors manual

## Table of Contents


###### \[in package FORMAT-ERRORS\]
A small Common Lisp library to output value and errors in a way that
appeals to me and is used in the field I currently work in.

<a id='x-28FORMAT-ERRORS-3AFORMAT-ERRORS-20FUNCTION-29'></a>

- [function] **FORMAT-ERRORS** *VALUE ERRORS &KEY (STREAM NIL) (DEFAULT-ERROR-DIGITS 2)*

    Given a value in `VALUE` and a simple list of errors in `ERRORS`,
    [`FORMAT-ERRORS`][6866] prints the value, list of errors, and the square root of
    the sum of the squares of the errors in delimiters (as defined by
    [`*ERROR-DELIMITERS*`][2fc5] and [`*SUM-ERROR-DELIMITERS*`][6989]) into `STREAM` (defaults
    to nil) with DEFAUL-ERROR-DIGITS (defaults to 2) number of digits. 
    Some examples:
    ```common-lisp
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
    ```

<a id='x-28FORMAT-ERRORS-3A-2ADECIMAL-MARK-2A-20-28VARIABLE-29-29'></a>

- [variable] **\*DECIMAL-MARK\*** *#\.*

    Character used to indicate decimal mark in number. Defaults to '.'

<a id='x-28FORMAT-ERRORS-3A-2AERROR-DELIMITERS-2A-20-28VARIABLE-29-29'></a>

- [variable] **\*ERROR-DELIMITERS\*** *"()"*

    Delimiters used to indicate individual errors.

<a id='x-28FORMAT-ERRORS-3A-2ASUM-ERROR-DELIMITERS-2A-20-28VARIABLE-29-29'></a>

- [variable] **\*SUM-ERROR-DELIMITERS\*** *"{}"*

    Delimiters used to indicate square root of the sum of the squares of the errors.

  [2fc5]: #x-28FORMAT-ERRORS-3A-2AERROR-DELIMITERS-2A-20-28VARIABLE-29-29 "(FORMAT-ERRORS:*ERROR-DELIMITERS* (VARIABLE))"
  [6866]: #x-28FORMAT-ERRORS-3AFORMAT-ERRORS-20FUNCTION-29 "(FORMAT-ERRORS:FORMAT-ERRORS FUNCTION)"
  [6989]: #x-28FORMAT-ERRORS-3A-2ASUM-ERROR-DELIMITERS-2A-20-28VARIABLE-29-29 "(FORMAT-ERRORS:*SUM-ERROR-DELIMITERS* (VARIABLE))"
