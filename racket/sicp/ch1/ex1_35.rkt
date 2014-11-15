#lang racket

(define fixpoint-torlerance 0.00001)
(define [close-enough? a b how-close]
  (< (abs (- a b)) how-close))

(define [fixed-point f init-guess]
  (define [iter cur-guess]
    (let ([next-guess (f cur-guess)])
      (if [close-enough? cur-guess next-guess fixpoint-torlerance]
        (/ (+ cur-guess next-guess) 2)
        (iter next-guess))))
  (iter init-guess))

(fixed-point cos 1.0)
(fixed-point (lambda (x) (+ (sin x) (cos x))) 1.0)
(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)
(fixed-point (lambda (x) (+ 1 (/ 1 x))) 0.5)
