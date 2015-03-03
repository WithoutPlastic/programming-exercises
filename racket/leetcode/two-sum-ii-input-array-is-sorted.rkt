#lang racket

;Problem:
;Two Sum II - Input array is sorted
;
;Given an array of integers that is already sorted in ascending order, find two
;numbers such that they add up to a specific target number.
;
;The function twoSum should return indices of the two numbers such that they
;add up to the target, where index1 must be less than index2. Please note that
;your returned answers (both index1 and index2) are not zero-based.
;
;You may assume that each input would have exactly one solution.
;
;Input: numbers={2, 7, 11, 15}, target=9
;Output: index1=1, index2=2

(require "lib/permutation.rkt")

(define [two-sum ints target]
  (let ([idx-pairs (pick-n-elts (range 0 (length ints)) 2)])
    (map
      (λ [p] (cons (add1 (car p)) (add1 (cadr p))))
      (filter
        (λ [p] [= (+ (list-ref ints (car p)) (list-ref ints (cadr p))) target])
        idx-pairs))))

(two-sum '(2 7 11 15) 9)
