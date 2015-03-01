#lang racket

;Problem:
;Given a triangle, find the minimum path sum from top to bottom. Each step you
;may move to adjacent numbers on the row below.
;
;For example, given the following triangle
;
;[
;     [2],
;    [3,4],
;   [6,5,7],
;  [4,1,8,3]
;]
;
;The minimum path sum from top to bottom is 11 (i.e., 2 + 3 + 5 + 1 = 11).
;
;Note:
;Bonus point if you are able to do this using only O(n) extra space, where n is
;the total number of rows in the triangle.

(define [minimum-total triangle]
  (define [iter remaining accum]
    (if [null? remaining] accum
      (let ([ext-ps (λ [ps n] (map (curry cons n) ps))])
        (iter (cdr remaining)
              (map append
                   (append (list '()) (map ext-ps accum (cdar remaining)))
                   (append (map ext-ps accum (drop-right (car remaining) 1))
                           (list '())))))))

  (apply min (append-map (λ [ps] (map (curry foldl + 0) ps))
                         (iter (cdr triangle) (list (list '(2)))))))

(define test-triangle (list '(2) '(3 4) '(6 5 7) '(4 1 8 3)))

(minimum-total test-triangle)
