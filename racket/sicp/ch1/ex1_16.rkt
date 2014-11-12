#lang racket

(define (is-even x) (= (/ x 2) 0))
(define (square x) (* x x))

(define (expt-iter b n merge)
  (cond 
    ((= n 0) merge)
    ((is-even n) (expt-iter (square b) (/ n 2) merge))
    (else (expt-iter b (- n 1) (* b merge)))))

(define (fast-expt b n)
  (expt-iter b n 1))

(fast-expt 2 0)
(fast-expt 2 1)
(fast-expt 2 2)
(fast-expt 2 3)
(fast-expt 2 4)

(fast-expt 2 20)
