#lang racket

;Problem:
;Implement int sqrt(int x).
;
;Compute and return the square root of x.

(define [sqrt-x int]
  (define init-int 1.0)
  (define precision 0.0000001)

  (unless [< int 0]
    (define [improve n]
      (let ([next (/ (+ (/ int n) n) 2)])
        (if [< (abs (- next n)) precision] next (improve next))))

    (if [= int 0] 0 (improve init-int))))

(sqrt-x 8)
(sqrt-x 4)
(sqrt-x 2)
(sqrt-x 1)
(sqrt-x 0.5)
(sqrt-x 0.00000009)
(sqrt-x 0)
(sqrt-x -1)
