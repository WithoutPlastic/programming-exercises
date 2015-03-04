#lang racket

;Problem:
;Given a sorted array and a target value, return the index if the target is
;found. If not, return the index where it would be if it were inserted in order.
;
;You may assume no duplicates in the array.
;
;Here are few examples.
;[1,3,5,6], 5 → 2
;[1,3,5,6], 2 → 1
;[1,3,5,6], 7 → 4
;[1,3,5,6], 0 → 0

(define [search-insert ints target]
  (let ([len (length ints)])
    (define [iter idx]
      (cond ([not [< idx len]] len)
            ([<= target (list-ref ints idx)] idx)
            (else (iter (add1 idx)))))

    (iter 0)))

(define test-ints '(1 3 5 6))
(search-insert test-ints 5)
(search-insert test-ints 2)
(search-insert test-ints 7)
(search-insert test-ints 0)
