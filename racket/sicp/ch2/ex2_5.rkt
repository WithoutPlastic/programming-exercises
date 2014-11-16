#lang racket

(define [cons a b]
  (*
    (expt 2 a)
    (expt 3 b)))

(define [car x]
  (define [iter left]
    (if [= (gcd 2 left) 1]
      0
      (+ (iter (/ left 2)) 1)))
  (iter x))

(define [cdr x]
  (define [iter left]
    (if [= (gcd 3 left) 1]
      0
      (+ (iter (/ left 3)) 1)))
  (iter x))

(car (cons 4 7))
(cdr (cons 4 7))
