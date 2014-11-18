#lang racket

(define x (list (list 1 2) (list 3 4)))

(define [fringe items]
  (define [iter items]
    (if [and (not (list? (car items))) (not (list? (car (cdr items))))]
      items
      (append (iter (car items)) (iter (car (cdr items))))))
  (iter items))

(fringe x)
(fringe (list x x))
