#lang racket

(define [make-accumulator accumulated-value]
  (lambda [add-value]
    (set! accumulated-value (+ add-value accumulated-value))
    accumulated-value))

(define A (make-accumulator 5))
(A 10)
(A 10)
