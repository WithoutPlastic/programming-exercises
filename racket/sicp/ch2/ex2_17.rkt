#lang racket

(define [last-pair items]
  (if [null? (cdr (cdr items))]
    items
    (last-pair (cdr items))))

(last-pair (list 23 72 149 34))
