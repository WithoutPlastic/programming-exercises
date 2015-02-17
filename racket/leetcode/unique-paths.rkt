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

(define [factorial n] (if [< 1 n] (* n (factorial (sub1 n))) 1))

(define [unique-paths m n]
  (let ([down-ops (sub1 m)] [right-ops (sub1 n)])
    (/ (factorial (+ down-ops right-ops))
       (* (factorial down-ops) (factorial right-ops)))))

(unique-paths 3 7)
