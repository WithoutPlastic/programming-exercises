#lang racket

;Problem:
;Divide two integers without using multiplication, division and mod operator.
;
;If it is overflow, return MAX_INT.

(define max-int 2147483647)

(define [divide-two-integers int d]
  (define [iter remaining result]
    (define [continue]
      (if [< remaining d] result (iter (- remaining d) (add1 result))))

    (if [< result max-int] (continue) max-int))

  (iter int 0))

(divide-two-integers 1024 2)
(divide-two-integers 10000000000 3)
