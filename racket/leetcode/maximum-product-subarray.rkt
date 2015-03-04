#lang racket

;Problem:
;Find the contiguous subarray within an array (containing at least one number)
;which has the largest product.
;
;For example, given the array [2,3,-2,4],
;the contiguous subarray [2,3] has the largest product = 6.

(require "lib/permutation.rkt")

(define [max-product ints]
  (let ([len (length ints)])
    (apply max (map (λ [p] (apply * (take (drop ints (car p)) (cdr p))))
                    (append-map
                      (λ [slice-len] (map (curryr cons slice-len)
                                          (range 0 (add1 (- len slice-len)))))
                      (range 1 (add1 len)))))))

(max-product '(2 3 -2 4))
