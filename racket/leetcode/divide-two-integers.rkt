#lang racket

;Problem:
;Divide two integers without using multiplication, division and mod operator.
;
;If it is overflow, return MAX_INT.

(define max-int 2147483647)

(define [divide-two-integers int d]
  (if [< int d] 0 (add1 (divide-two-integers (- int d) d))))

(divide-two-integers 1024 2)
(divide-two-integers 10000000000 3)
