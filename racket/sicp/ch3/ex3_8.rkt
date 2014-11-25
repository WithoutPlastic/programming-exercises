#lang racket

(define called-times 0)
(define [f x]
  (begin
    (set! called-times (add1 called-times))
    (if [= called-times 1]
      x
      0)))

(+ (f 1) (f 0))

(set! called-times 0)
(+ (f 0) (f 1))
