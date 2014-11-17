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
  (if [(and (< (lower-bound a-interval) 0) (< (0 (upper-bound a-interval))))]
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

(define [new-interval-mul x y]
  (cond
    ([and
       (< (upper-bound x) 0)
       (< (upper-bound y) 0)]
     (make-interval
       (* (upper-bound x) (upper-bound y))
       (* (lower-bound x) (lower-bound y))))
    ([and
       (< (upper-bound x) 0)
       (< 0 (upper-bound y))]
     (make-interval
       (* (lower-bound x) (upper-bound y))
       (* (lower-bound x) (lower-bound y))))
    ([and
       (< (upper-bound x) 0)
       (< 0 (lower-bound y))]
     (make-interval
       (* (lower-bound x) (upper-bound y))
       (* (lower-bound x) (lower-bound y))))
    ;and so on
    ))
;<-----------0----------->
;(a, b)(c, d)|
;(a, b)   (c,| d)
;(a, b)      | (c, d)
;(c, d)   (a,| b)
;         (ac,| bd)
;         (a,| b) (c, d)
;(c, d)      | (a, b)
;         (c,| d) (a, b)
;            | (a, b) (c, d)
