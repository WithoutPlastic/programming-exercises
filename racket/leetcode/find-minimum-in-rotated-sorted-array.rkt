#lang racket

;Problem:
;Suppose a sorted array is rotated at some pivot unknown to you beforehand.
;
;(i.e., 0 1 2 4 5 6 7 might become 4 5 6 7 0 1 2).
;
;Find the minimum element.
;
;You may assume no duplicate exists in the array.

(define [find-min r-ints]
  (cdr (findf (λ [p] [< (cdr p) (car p)])
              (map cons r-ints (append (drop r-ints 1) (list (car r-ints)))))))

(find-min '(4 5 6 7 0 1 2))
