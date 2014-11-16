#lang racket

(define [numer x] (car x))
(define [denom x] (cdr x))
(define [make-rat n d] (cons n d))

(define [remove-gcd rat-out-prod]
  (lambda (n d)
    (let ([g (gcd n d)])
      (rat-out-prod (/ n g) (/ d g)))))
(define no-gcd-make-rat (remove-gcd make-rat))

(define [move-minus rat-out-prod]
  (lambda (n d)
    (cond ([< d 0] (rat-out-prod (- n) (- d)))
          (else (rat-out-prod n d)))))
(define moved-minus-make-rat (move-minus make-rat))

(define new-make-rat (move-minus (remove-gcd make-rat)))

(define [add-rat x y]
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define [sub-rat x y]
  (make-rat (- (* numer x) (denom y)
               (* numer y) (denom x))
            (* (denom x) (denom y))))

(define [mul-rat x y]
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define [div-rat x y]
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define [rat-equal? x y]
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define [print-rat x]
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(print-rat (make-rat 3 -9))
(print-rat (no-gcd-make-rat 3 -9))
(print-rat (moved-minus-make-rat 3 -9))
(print-rat (new-make-rat 3 -9))
