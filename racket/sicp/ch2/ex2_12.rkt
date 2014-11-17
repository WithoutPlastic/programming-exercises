#lang racket

(define [make-cw-interval center width]
  (cons
    (- center width)
    (+ center width)))

(define [cw-interval-center a-interval]
  (/
    (+
      (car a-interval)
      (cdr a-interval))
    2))

(define [cw-interval-width a-interval]
  (/
    (-
      (cdr a-interval)
      (car a-interval))
    2))

(define [make-cp-interval center percent]
  (cons
    (- center (* center percent))
    (+ center (* center percent))))

(define [cp-interval-center a-interval]
  (/
    (+
      (car a-interval)
      (cdr a-interval))
    2))

(define [cp-interval-percent a-interval]
  (/
    (/
      (-
        (cdr a-interval)
        (car a-interval))
      2)
    (cp-interval-center a-interval)))
