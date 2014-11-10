#lang racket

(define result-iter-limit 0.000001)
(define init-stub 1.0)

(define (abs x)
  (if (< x 0) (- x) x))

(define (squart x)
  (* x x))

(define (result-var-under-limit new-result result)
  (< (abs (- (/ new-result result) 1.0))
     result-iter-limit))

(define (improve-result result given-number)
  (/ (+ (* 2 result) (/ given-number (squart result))) 3))

(define (iter-cube-root result given-number)
  (if (result-var-under-limit
        (improve-result result given-number)
        result)
    result
    (iter-cube-root
      (improve-result result given-number)
      given-number)))

(define (compute-cube-root given-number)
  (iter-cube-root init-stub given-number))

(compute-cube-root 27)
