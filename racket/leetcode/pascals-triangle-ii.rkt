#lang racket

;Problem:
;Given an index k, return the kth row of the Pascal's triangle.
;
;For example, given k = 3,
;Return [1,3,3,1].
;
;Note:
;Could you optimize your algorithm to use only O(k) extra space?

(define [pascal-triangle-row n]
  (define [iter prow cnt]
    (if [= 0 cnt] prow
      (iter (map + (cons 0 prow) (reverse (cons 0 prow))) (sub1 cnt))))

  (when [< 0 n] (iter '(1) (- n 1))))

(pascal-triangle-row 5)
(pascal-triangle-row 8)
