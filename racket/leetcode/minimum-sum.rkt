#lang racket

;Problem:
;Given a m x n grid filled with non-negative numbers, find a path from top left
;to bottom right which minimizes the sum of all numbers along its path.
;
;Note: You can only move either down or right at any point in time.

(require "permutations-ii.rkt")

(define down #\d)
(define right #\r)

(define [ops->index-pairs ops]
  (define [iter ops pos]
    (define [continue]
      (let ([cur-op (car ops)] [pos-row (car pos)]
            [pos-col (cdr pos)] [rest-ops (cdr ops)])
        (if [eq? cur-op down]
          (iter rest-ops (cons (add1 pos-row) pos-col))
          (iter rest-ops (cons pos-row (add1 pos-col))))))

    (if [null? ops] (list pos) (cons pos (continue))))

  (iter ops '(0 . 0)))

(define [min-path-sum grid]
  (define [path-sum ops]
    (let ([idx-pairs (ops->index-pairs ops)])
      (apply + (map (lambda [p] (list-ref (list-ref grid (car p)) (cdr p)))
                    idx-pairs))))

  (let* ([row (length grid)]
         [col (length (car grid))]
         [steps (append (make-list (- row 1) down) (make-list (- col 1) right))]
         [all-paths (permute-unique steps)])
    (apply min (map path-sum all-paths))))

(define test-grid
  (list '(0 1 8 4 2 -1 8)
        '(1 1 3 6 -3 8 1)
        '(-1 -4 2 4 6 8 0)))

(min-path-sum test-grid)
