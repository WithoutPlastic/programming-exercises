#lang racket

;Problem:
;Given n points on a 2D plane, find the maximum number of points that lie on
;the same straight line.

(require "lib/permutation.rkt")

(define [max-points points]
  (define make-line cons) (define line-k car) (define line-a cdr)
  (define make-point cons) (define axis-x car) (define axis-y cdr)
  (define [points->line p-a p-b]
    (let* ([Δx (- (axis-x p-a) (axis-x p-b))]
           [Δy (- (axis-y p-a) (axis-y p-b))]
           [k (if [= Δx 0] +inf.0 (/ Δy Δx))]
           [a (- (axis-y p-a) (* (axis-x p-a) k))])
      (make-line k a)))
  (define [point-on-line? line point]
    [= (axis-y point) (+ (* (line-k line) (axis-x point)) (line-a line))])

  (let* ([point-pairs (pick-n-elts (remove-duplicates points) 2)]
         [lines (map (curry apply points->line) point-pairs)]
         [unique-lines (remove-duplicates lines)]
         [count-points-on-line? (λ [l] (count (curry point-on-line? l) points))]
         [line-counts (map count-points-on-line? unique-lines)])
    (apply max line-counts)))

(max-points (list '(1 . 0) '(2 . 1) '(5 . 2) '(2 . 3) '(4 . 3) '(0 . -1)))
