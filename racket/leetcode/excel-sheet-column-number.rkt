#lang racket

;Problem:
;Related to question Excel Sheet Column Title
;
;Given a column title as appear in an Excel sheet, return its corresponding
;column number.
;
;For example:
;
;  A -> 1
;  B -> 2
;  C -> 3
;  ...
;  Z -> 26
;  AA -> 27
;  AB -> 28
;
;Credits:
;Special thanks to @ts for adding this problem and creating all test cases.

(define [alpha-title->integer str]
  (define alpha-integer-table
    (map cons (map integer->char (range 65 91)) (range 1 27)))

  (define [iter chars]
    (if [null? chars] 0
      (+ (cdr (assoc (car chars) alpha-integer-table))
         (* 26 (iter (cdr chars))))))

  (iter (reverse (string->list str))))

(alpha-title->integer "C")
(alpha-title->integer "AB")
