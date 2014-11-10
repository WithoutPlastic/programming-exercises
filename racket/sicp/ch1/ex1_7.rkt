#lang racket

(define var-part-small-enough 0.000001)
(define init-stub 1.0)

(define (avg x y) 
  (/ (+ x y) 2))

(define (abs x) 
  (if (< x 0) 
    (- x) 
    x))

(define (verdict-close-enough result input)
  (< (abs (- (/ (improve-result result input) result) 1.0)) var-part-small-enough))

(define (improve-result result input)
  (/ (+ result (/ input result))
     2))

(define (iter-sqrt-result result input)
  (if (verdict-close-enough result input)
    result
    (iter-sqrt-result 
      (improve-result result input) 
      input)))

(define (compute-sqrt input)
  (iter-sqrt-result init-stub input))

(compute-sqrt 9)
(compute-sqrt 13)
(compute-sqrt 2)
(compute-sqrt 3)
(compute-sqrt 0.09)
(compute-sqrt 0.000000000000000000000009)
