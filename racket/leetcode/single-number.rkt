#lang racket

;Problem:
;Given an array of integers, every element appears twice except for one. Find
;that single one.
;
;Note:
;Your algorithm should have a linear runtime complexity. Could you implement it
;without using extra memory?

(define [single-number ints] (foldl bitwise-xor 0 ints))

(single-number '(1 2 3 4 5 5 4 3 2 8 1))
