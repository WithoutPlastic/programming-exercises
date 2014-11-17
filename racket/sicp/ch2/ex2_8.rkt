#lang racket

(define [make-interval bound-a bound-b]
  (cons (min bound-a bound-b) (max bound-a bound-b)))
(define [lower-bound a-interval] (car a-interval))
(define [upper-bound a-interval] (cdr a-interval))

(define [interval-add x y]
  (make-interval
    (+ (lower-bound x) (lower-bound y))
    (+ (upper-bound x) (upper-bound y))))

(define [interval-sub x y]
  (make-interval
    (- (lower-bound x) (upper-bound y))
    (- (upper-bound x) (lower-bound y))))

(define [interval-mul x y]
  (let ([p1 (* (lower-bound x) (lower-bound y))]
        [p2 (* (lower-bound x) (upper-bound y))]
        [p3 (* (upper-bound x) (lower-bound y))]
        [p4 (* (upper-bound x) (upper-bound y))])
    (make-interval
      (min p1 p2 p3 p4)
      (max p1 p2 p3 p4))))

(define [interval-div x y]
  (interval-mul
    x
    (make-interval
      (/ 1.0 (upper-bound y))
      (/ 1.0 (lower-bound y)))))
