#lang racket

(define zero (lambda (f) (lambda (x) x)))
(define [add-one n] (lambda (f) (lambda (x) (f ((n f) x)))))
(define [plus n m] (lambda (f) (lambda (x) ((n f) ((m f) x)))))
(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))
(define three (lambda (f) (lambda (x) (f (f (f x))))))

(define meta-f (lambda (x) (* x 2)))
(define meta-rf (lambda (x) (/ x 2)))

;(define zero (lambda (g) (lambda (f) (lambda (x) x))))
((one meta-f) 1)
(((add-one zero) meta-f) 1)

((two meta-f) 1)
(((add-one one) meta-f) 1)

(((plus two two) meta-f) 1)
(((plus (plus two two) one) meta-f) 1)

