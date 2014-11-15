#lang racket

(define [square x] (* x x))

(define [recur-product calculator from to next finished?]
  (define [iter current]
    (if [finished? current to]
      (* 1.0 current)
      (* (calculator current) (iter (next current)))))
  (iter from))

(define [iter-product calculator from to next finished?]
  (define [iter current result]
    (if [finished? current to]
      (* 1.0 current)
      (iter (next current) result)))
  (iter from 1.0))

(define [pi-calculator x]
  (square (/ x (+ x 1))))

(define [inc-two x] (+ x 2))

(define half-pi-via-recur
  (recur-product pi-calculator 2 1000000 inc-two >=))

(define half-pi-via-iter
  (recur-product pi-calculator 2 1000000 inc-two >=))

half-pi-via-recur
half-pi-via-iter
