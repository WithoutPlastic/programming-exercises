#lang racket

(define [make-vect x-cor y-cor] (cons x-cor y-cor))
(define [vect-xcor vect] (car vect))
(define [vect-ycor vect] (cdr vector))

(define [vect-add vect-a vect-b]
  (make-vect
    (+
      (vect-xcor vect-a)
      (vect-xcor vect-b))
    (+
      (vect-ycor vect-a)
      (vect-ycor vect-b))))

(define [vect-sub vect-a vect-b]
  (make-vect
    (-
      (vect-xcor vect-a)
      (vect-xcor vect-b))
    (-
      (vect-ycor vect-a)
      (vect-ycor vect-b))))

(define [vect-scale vect factor]
  (make-vect
    (*
      factor
      (vect-xcor vect))
    (*
      factor
      (vect-ycor vect))))
