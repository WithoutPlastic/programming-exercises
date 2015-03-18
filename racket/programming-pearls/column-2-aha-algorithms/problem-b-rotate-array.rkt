#lang racket

(define [shift-left lst] (append (cdr lst) (list (car lst))))
(define [shift-right lst] (cons (last lst) (drop-right lst 1)))

(define [rotate.v1 lst k] (if [= 0 k] lst (rotate.v1 (shift-right lst) (sub1 k))))

(define [rotate.v2 lst k] (append (take-right lst k) (drop-right lst k)))

(define [rotate.v3 lst k]
  (call-with-values (λ [] (split-at-right lst k))
                    (λ [h t] (reverse (append (reverse h) (reverse t))))))

(define test-lst (range 0 20))

(rotate.v1 test-lst 5)
(rotate.v1 test-lst 15)
(rotate.v2 test-lst 5)
(rotate.v2 test-lst 15)
(rotate.v3 test-lst 5)
(rotate.v3 test-lst 15)
