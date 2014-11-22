#lang racket

(define [equ? value-a value-b]
  (apply-generic 'equ? value-a value-b))

(define [install-number-package]
  ;...
  (put 'equ? '(scheme-number scheme-number)
       (lambda [value-a value-b]
         (= value-a value-b))))

(define [install-rational-package]
  ;...
  (put 'equ? '(rational rational)
       (lambda [value-a value-b]
         (and
           (= (numer value-a) (numer value-b))
           (= (denom value-a) (denom value-b))))))

(define [install-complex-package]
  ;...
  (put 'equ? '(complex complex)
       (lambda [value-a value-b]
         (and
           (= (magnitude value-a) (magnitude value-b))
           (= (angle value-a) (angle value-b)))))

  (put 'equ? '(complex complex)
       (lambda [value-a value-b]
         (and
           (= (real-part value-a) (real-part value-b))
           (= (imag-part value-a) (imag-part value-b)))))
  )
