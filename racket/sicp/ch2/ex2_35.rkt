#lang racket

(define [count-leaves tree]
  (accumulate
    (lambda (x y) (+ x y))
    0
    (map (lambda (x) (length (enumerate-tree x))) tree)))
