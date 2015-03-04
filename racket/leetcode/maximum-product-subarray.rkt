#lang racket

;Problem:
;Find the contiguous subarray within an array (containing at least one number)
;which has the largest product.
;
;For example, given the array [2,3,-2,4],
;the contiguous subarray [2,3] has the largest product = 6.

(require "lib/permutation.rkt")

(define [max-product nums]
  (define [iter remaining min-p max-p result]
    (if [null? remaining] result
      (let* ([first-n (car remaining)] [rests (cdr remaining)]
             [next-min-p (min (* min-p first-n) (* min-p first-n) first-n)]
             [next-max-p (max (* max-p first-n) (* max-p first-n) first-n)]
             [next-result (max next-max-p result)])
        (iter rests next-min-p next-max-p next-result))))

  (if [null? nums] 0 (iter nums 1 1 1)))

(max-product '(2 3 -2 4))
(max-product '(2 4 -3 6 9 7 3 -1 2 1/2 3 -2 6 4 -3 2 7 8 -1 9 2 3))
