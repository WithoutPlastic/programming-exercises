#lang racket

;Problem:
;Given an integer n, return the number of trailing zeroes in n!.
;
;Note: Your solution should be in logarithmic time complexity.
;
;Credits:
;Special thanks to @ts for adding this problem and creating all test cases.

(define [trailing-zeroes n]
  (define [iter base]
    (if [< (floor (/ n base)) 1] 0 (+ (floor (/ n base)) (iter (* base 5)))))

  (iter 5))

(trailing-zeroes 5)
(trailing-zeroes 25)
(trailing-zeroes 100)
