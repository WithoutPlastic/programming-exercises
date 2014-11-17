#lang racket

(define [last-pair items]
  (if [null? (cdr (cdr items))]
    items
    (last-pair (cdr items))))

(define [reverse items]
  (if [null? items]
    items
    (append (reverse (cdr items)) (list (car items)))))

(reverse (list 1 4 9 16 25))
