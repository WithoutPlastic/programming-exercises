#lang racket

;Problem:
;A robot is located at the top-left corner of a m x n grid (marked 'Start' in
;the diagram below).
;
;The robot can only move either down or right at any point in time. The robot is
;trying to reach the bottom-right corner of the grid (marked 'Finish' in the
;diagram below).
;
;How many possible unique paths are there?
;
;Above is a 3 x 7 grid. How many possible unique paths are there?
;
;Note: m and n will be at most 100.

(require "permutations-ii.rkt")

(define down #\d)
(define right #\r)

(define [unique-paths m n]
  (let ([ops (append (make-list (sub1 m) down) (make-list (sub1 n) right))])
    (length (permute-unique ops))))

(unique-paths 3 7)
