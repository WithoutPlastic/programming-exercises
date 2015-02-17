#lang racket

;Problem:
;Find the contiguous subarray within an array (containing at least one number)
;which has the largest sum.
;
;For example, given the array [−2,1,−3,4,−1,2,1,−5,4],
;the contiguous subarray [4,−1,2,1] has the largest sum = 6.
;
;More practice:
;If you have figured out the O(n) solution, try coding another solution using
;the divide and conquer approach, which is more subtle.

(define [max-sub-list ints]
  (let ([len (length ints)])
    (define [iter cur-idx cur-accum m-head m-tail m-accum]
      (define [continue]
        (let* ([cur-int (list-ref ints cur-idx)]
               [next-accum (+ cur-int cur-accum)]
               [next-idx (add1 cur-idx)])
          (cond ([< next-accum 0]
                 (iter next-idx 0 next-idx m-tail m-accum))
                ([< m-accum next-accum]
                 (iter next-idx next-accum m-head cur-idx next-accum))
                (else (iter next-idx next-accum m-head m-tail m-accum)))))

      (if [< cur-idx len] (continue) (list m-head m-tail m-accum)))

    (iter 0 0 0 0 0)))

(define test-array '(-2 1 -3 4 -1 2 1 -5 4))

(max-sub-list test-array)
