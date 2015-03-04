#lang racket

;Problem:
;Given two integers representing the numerator and denominator of a fraction,
;return the fraction in string format.
;
;If the fractional part is repeating, enclose the repeating part in parentheses.
;
;For example,
;
; - Given numerator = 1, denominator = 2, return "0.5".
; - Given numerator = 2, denominator = 1, return "2".
; - Given numerator = 2, denominator = 3, return "0.(6)".
;
;Credits:
;Special thanks to @Shangrila for adding this problem and creating all test
;cases.

(require "lib/char-number-convert.rkt")

(define [fraction-to-decimal num den]
  (define [fraction-part num]
    (if [= num 0] '()
      (let* ([lnum (* num 10)]
             [rnum (remainder lnum den)]
             [result (floor (/ lnum den))]
             [result-char (number->char result)]
             [next-result (floor (/ (* rnum 10) den))])
        (if [= next-result result] `(#\( ,result-char #\))
          (cons result-char (fraction-part rnum))))))

  (let ([rnum (remainder num den)])
    (list->string
      (append (number->chars (floor (/ num den)))
              (if [= rnum 0] '() (append '(#\.) (fraction-part rnum)))))))

(fraction-to-decimal 1 2)
(fraction-to-decimal 2 1)
(fraction-to-decimal 2 3)
(fraction-to-decimal 3 2)
