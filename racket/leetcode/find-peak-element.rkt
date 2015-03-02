#lang racket

;Problem:
;A peak element is an element that is greater than its neighbors.
;
;Given an input array where num[i] ≠ num[i+1], find a peak element and return
;its index.
;
;The array may contain multiple peaks, in that case return the index to any one
;of the peaks is fine.
;
;You may imagine that num[-1] = num[n] = -∞.
;
;For example, in array [1, 2, 3, 1], 3 is a peak element and your function
;should return the index number 2.
;
;click to show spoilers.
;
;Credits:
;Special thanks to @ts for adding this problem and creating all test cases.

(define [find-peak-element ints]
  (if [<= (length ints) 2] -1
    (let* ([<> (drop-right ints 2)]
           [<<<< (cddr ints)]
           [<< (drop-right (cdr ints) 1)]
           [tri-pairs (map list (range 0 (- (length ints) 2)) <> << <<<<)]
           [tri-pair-peak? (λ [p] [< (max (cadr p) (cadddr p)) (caddr p)])])
      (map car (filter tri-pair-peak? tri-pairs)))))

(define test-ints (build-list 10 (thunk* (random 10))))

(displayln test-ints)
(find-peak-element test-ints)
