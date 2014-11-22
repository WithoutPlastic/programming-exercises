#lang racket

(define [equ? value-a value-b]
  (apply-generic 'equ? value-a value-b))

(define [zero? value]
  (apply-generic '=zero? value))

(define [install-number-package]
  ;...
  (put 'equ? '(scheme-number scheme-number)
       (lambda [value-a value-b]
         (= value-a value-b)))
  (put '=zero? '(scheme-number)
       (lambda [value]
         (= value 0))))

(define [install-rational-package]
  ;...
  (put 'equ? '(rational rational)
       (lambda [value-a value-b]
         (and
           (= (numer value-a) (numer value-b))
           (= (denom value-a) (denom value-b)))))
  (put '=zero? '(rational)
       (lambda [value]
         (= (numer value) 0))))

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

  (put '=zero? '(complex)
       (lambda [value]
         (= (magnitude value) 0)))
  (put '=zero? '(complex)
       (lambda [value]
         (and
           (= (real-part value) 0)
           (= (imag-part value) 0)))))
