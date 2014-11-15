#lang racket

(define [square x] (* x x))
(define [recur-product calculator from to next finished? finalized-value]
  (recur-accumulator * calculator from to next finished? finalized-value))

(define [iter-product calculator from to next finished? finalized-value]
  (iter-accumulator * calculator from to next finished? finalized-value))

(define [recur-accumulator combiner calculator from to next finished? finalized-value]
  (define [iter current]
    (if [finished? current to]
      finalized-value
      (combiner
        (calculator current)
        (iter (next current)))))
  (iter from))

(define [iter-accumulator combiner calculator from to next finished? finalized-value]
  (define [iter current result]
    (if [finished? current to]
      (combiner finalized-value result)
      (iter (next current) (combiner (calculator current) result))))
  (iter from 1))

(define recur-half-pi
  (recur-product
    (lambda (x) (square (/ x (+ x 1))))
    2
    1000000
    (lambda (x) (+ x 2))
    >=
    1000000.0
    ))

(define iter-half-pi
  (iter-product
    (lambda (x) (square (/ x (+ x 1))))
    2
    1000000
    (lambda (x) (+ x 2))
    >=
    1000000.0
    ))

recur-half-pi
iter-half-pi
