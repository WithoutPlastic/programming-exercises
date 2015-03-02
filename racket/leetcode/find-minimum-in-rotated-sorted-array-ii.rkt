#lang racket

;Problem:
;Follow up for "Find Minimum in Rotated Sorted Array":
;What if duplicates are allowed?
;
;Would this affect the run-time complexity? How and why?
;
;Suppose a sorted array is rotated at some pivot unknown to you beforehand.
;
;(i.e., 0 1 2 4 5 6 7 might become 4 5 6 7 0 1 2).
;
;Find the minimum element.
;
;The array may contain duplicates.

(define [find-min r-ints]
  (let* ([<< (append (drop r-ints 1) (list (car r-ints)))]
         [min-elt (findf (λ [p] [< (cdr p) (car p)]) (map cons r-ints <<))])
    (if min-elt (cdr min-elt) (car r-ints))))

(find-min '(4 5 6 7 0 1 2))
(find-min '(0 0 0 0 1 0 0 0 0))
(find-min '(0 0 0 0 -3 -1 0 0 0))
(find-min '(0 0 0 0 0 0 0 0 0))
