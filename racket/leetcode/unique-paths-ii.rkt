#lang racket

;Problem:
;Follow up for "Unique Paths":
;
;Now consider if some obstacles are added to the grids. How many unique paths
;would there be?
;
;An obstacle and empty space is marked as 1 and 0 respectively in the grid.
;
;For example,
;There is one obstacle in the middle of a 3x3 grid as illustrated below.
;
;[
;[0,0,0],
;[0,1,0],
;[0,0,0]
;]
;
;The total number of unique paths is 2.
;
;Note: m and n will be at most 100.

(require "permutations-ii.rkt")

(define down #\d)
(define right #\r)

(define [prefix? sub-lst full-lst]
  (define [iter s-remaining f-remaining]
    (cond ([null? s-remaining] true)
          ([eq? (car s-remaining) (car f-remaining)]
           (iter (cdr s-remaining) (cdr f-remaining)))
          (else false)))

  (iter sub-lst full-lst))

(define [unique-paths-with-obstacles obstacle-grid]
  (define [grid-value p] (list-ref (list-ref obstacle-grid (car p)) (cdr p)))
  (define [all-available-idxs m n]
    (append-map (lambda [r] (map (lambda [c] (cons r c)) (range 0 n)))
                (range 0 m)))
  (define [op-permutations d-op-times r-op-times]
    (permute-unique (append (make-list d-op-times down)
                            (make-list r-op-times right))))

  (let* ([m (length obstacle-grid)]
         [n (length (car obstacle-grid))]
         [all-op-permutations (op-permutations (sub1 m) (sub1 n))]
         [all-obstacle-idxs (filter (lambda [p] [= (grid-value p) 1])
                                    (all-available-idxs m n))]
         [ob-op-permutations
           (append-map (lambda [p] (op-permutations (car p) (cdr p)))
                       all-obstacle-idxs)])

    (length
      (filter-not (lambda [ops] [ormap (curryr prefix? ops) ob-op-permutations])
                  all-op-permutations))))

(define test-grid-a (list '(0 0 0) '(0 1 0) '(0 0 0)))
(define test-grid-b (list '(0 0 0 0) '(0 0 1 0) '(0 0 0 0)))
(define test-grid-c (list '(0 0 0 0) '(0 1 1 0) '(0 0 0 0)))
(define test-grid-d (list '(0 0 0 0) '(1 1 1 0) '(0 1 0 0)))

(unique-paths-with-obstacles test-grid-a)
(unique-paths-with-obstacles test-grid-b)
(unique-paths-with-obstacles test-grid-c)
(unique-paths-with-obstacles test-grid-d)
