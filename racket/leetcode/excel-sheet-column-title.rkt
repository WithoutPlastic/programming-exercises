#lang racket

;Problem:
;Given a positive integer, return its corresponding column title as appear in
;an Excel sheet.
;
;For example:
;
;  1 -> A
;  2 -> B
;  3 -> C
;  ...
;  26 -> Z
;  27 -> AA
;  28 -> AB
;
;Credits:
;Special thanks to @ifanchu for adding this problem and creating all test cases.

(define [integer->alpha-title int]
  (define integer-alpha-table
    (map cons (range 0 26) (map integer->char (range 65 91))))

  (if [= int 0] '()
    (append (integer->alpha-title (floor (/ (- int 1) 26)))
            (list (cdr (assoc (remainder (- int 1) 26) integer-alpha-table))))))

(integer->alpha-title 1)
(integer->alpha-title 28)
