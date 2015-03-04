#lang racket

;Problem:
;Given a sorted array of integers, find the starting and ending position of a
;given target value.
;
;Your algorithm's runtime complexity must be in the order of O(log n).
;
;If the target is not found in the array, return [-1, -1].
;
;For example,
;Given [5, 7, 7, 8, 8, 10] and target value 8, return [3, 4].

(define [search-range s-nums target]
  (let ([len (length s-nums)])
    (define [iter s-idx]
      (define [matching e-idx]
        (if [and [< e-idx len] [= (list-ref s-nums e-idx) target]]
          (matching (add1 e-idx))
          (sub1 e-idx)))

      (cond ([and [< s-idx len] [= (list-ref s-nums s-idx) target]]
             (cons s-idx (matching (add1 s-idx))))
            ([< s-idx len] (iter (add1 s-idx)))
            (else (cons -1 -1))))

    (iter 0)))

(define test-nums '(5 7 7 8 8 10))
(define test-target 8)

(search-range test-nums test-target)
