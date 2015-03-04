#lang racket

;Problem:
;Given an integer n, generate a square matrix filled with elements from 1 to n^2
;in spiral order.
;
;For example,
;
;Given n = 3, You should return the following matrix:
;
;[
;[ 1, 2, 3 ],
;[ 8, 9, 4 ],
;[ 7, 6, 5 ]
;]

(require "spiral-matrix.rkt")

(define [generate-matrix n]
  (let* ([vals (range 1 (add1 (* n n)))]
         [spiral-idxs (generate-spiral-idxs n n)]
         [s-idxs-with-val (map cons spiral-idxs vals)])
    (map (lambda [r]
           (map cdr (sort (filter (lambda [x] [= (caar x) r]) s-idxs-with-val)
                          (lambda [l r] [< (cdar l) (cdar r)]))))
         (range 0 n))))

(generate-matrix 3)
(generate-matrix 5)
