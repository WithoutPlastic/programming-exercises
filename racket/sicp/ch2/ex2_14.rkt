#lang racket

(define [make-interval bound-a bound-b]
  (cons (min bound-a bound-b) (max bound-a bound-b)))
(define [lower-bound a-interval] (car a-interval))
(define [upper-bound a-interval] (cdr a-interval))
(define [interval-wide a-interval]
  (/
    (-
      (upper-bound a-interval)
      (lower-bound a-interval))
    2))
(define [across-zero? a-interval]
  (if [and (< (lower-bound a-interval) 0) (< (0 (upper-bound a-interval)))]
    (error "Interval contains zero!")
    #f))

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
  (if [across-zero? y]
    (error "Interval value division failed.")
    (interval-mul
      x
      (make-interval
        (/ 1.0 (upper-bound y))
        (/ 1.0 (lower-bound y))))))

(define [make-cw-interval center width]
  (make-interval
    (- center width)
    (+ center width)))

(define [cw-interval-center a-interval]
  (/
    (+
      (lower-bound a-interval)
      (upper-bound a-interval))
    2))

(define [cw-interval-width a-interval]
  (/
    (-
      (upper-bound a-interval)
      (lower-bound a-interval))
    2))

(define [make-cp-interval center percent]
  (make-interval
    (- center (* center percent))
    (+ center (* center percent))))

(define [cp-interval-center a-interval]
  (/
    (+
      (lower-bound a-interval)
      (upper-bound a-interval))
    2))

(define [cp-interval-percent a-interval]
  (/
    (/
      (-
        (upper-bound a-interval)
        (lower-bound a-interval))
      2)
    (cp-interval-center a-interval)))

(define [par1 r1 r2]
  (interval-div
    (interval-mul r1 r2)
    (interval-add r1 r2)))

(define [par2 r1 r2]
  (let
    ([one (make-interval 1 1)])
    (interval-div
      one
      (interval-add
        (interval-div
          one r1)
        (interval-div
          one r2)))))

(par1 (make-cp-interval 1 0.05)
      (make-cp-interval 2 0.02))
(par2 (make-cp-interval 1 0.05)
      (make-cp-interval 2 0.02))
