#lang racket

(define meta-f (lambda (x) (* x 2)))
(define meta-rf (lambda (x) (/ x 2)))

(define zero
  (lambda (f) (lambda (rf) (lambda (x) x))))
(define [add-one n]
  (lambda (f) (lambda (rf) (lambda (x) (f (((n f) rf) x))))))
(define [sub-one n]
  (lambda (f) (lambda (rf) (lambda (x) (rf (((n f) rf) x))))))
(define [plus n m]
  (lambda (f) (lambda (rf) (lambda (x) (((n f) rf) (((m f) rf) x))))))
(define [sub n m]
  (lambda (f) (lambda (rf) (lambda (x) (((n f) rf) (((m rf) f) x))))))
(define one (add-one zero))
(define two (add-one (add-one zero)))
(define three (add-one (add-one (add-one zero))))
(define four (add-one (add-one (add-one (add-one zero)))))
(define minus-one (sub-one zero))
(define minus-two (sub-one (sub-one zero)))
(define minus-three (sub-one (sub-one (sub-one zero))))
(define minus-four (sub-one (sub-one (sub-one (sub-one zero)))))

(((one meta-f) meta-rf) 1)
((((add-one zero) meta-f) meta-rf) 1)

(((two meta-f) meta-rf) 1)
((((add-one one) meta-f) meta-rf) 1)
((((sub-one two) meta-f) meta-rf) 1)

((((plus two two) meta-f) meta-rf) 1)
((((plus (plus two two) one) meta-f) meta-rf) 1)
((((sub four two) meta-f) meta-rf) 1)
