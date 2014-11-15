#lang racket

(define [iterative-improve good-enough? improve]
  (define [iter current-guess]
    (let ([next-guess (improve current-guess)])
      (if [good-enough? current-guess next-guess]
        current-guess
        (iter (improve current-guess)))))
  (lambda (init-guess) (iter init-guess)))

(define [sqrt-with-iter number]
  ((iterative-improve (lambda (a b) (< (abs (- a b)) 0.0001))
                      (lambda (x) (/ (+ x (/ number x)) 2)))
   1.0))

(define [fixed-point f init-guess]
  ((iterative-improve (lambda (a b) (< (abs (- a b)) 0.0001))
                      (lambda (x) (f x)))
   init-guess))

(sqrt-with-iter 2)
(fixed-point sqrt-with-iter 0.5)

