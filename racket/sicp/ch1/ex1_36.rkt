#lang racket

(define fixpoint-torlerance 0.00001)
(define [close-enough? a b how-close]
  (< (abs (- a b)) how-close))
(define [average a b] (/ (+ a b) 2))

(define [fixed-point f init-guess]
  (define [iter cur-guess]
    (let ([next-guess (f cur-guess)])
      (if [close-enough? cur-guess next-guess fixpoint-torlerance]
        (/ (+ cur-guess next-guess) 2)
        (begin
          (display cur-guess)
          (newline)
          (iter next-guess)))))
  (iter init-guess))

;(fixed-point (lambda (x) (+ 1 (/ 1 x))) 0.5)
(fixed-point (lambda (x) (/ (log 1000) (log x))) 2)
(newline)
(fixed-point (lambda (x) (average (/ (log 1000) (log x)) x)) 2)
