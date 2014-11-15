#lang racket

(define [repeat-f f times]
  (define [repeat times]
    (if [= times 1]
      f
      (lambda (x) (f ((repeat (- times 1)) x)))))
  (repeat times))

((repeat-f (lambda (x) (* x x)) 3) 5)
