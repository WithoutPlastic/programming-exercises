#lang racket

;Problem:
;Given an unsorted integer array, find the first missing positive integer.
;
;For example,
;Given [1,2,0] return 3,
;and [3,4,-1,1] return 2.
;
;Your algorithm should run in O(n) time and uses constant space.

(define [first-missing-positive nums]
  (let* ([max-num (apply max nums)]
         [sum (/ (* max-num (add1 max-num)) 2)])
    (define [iter remaining left-sum]
      (define [continue]
        (let ([f-num (car remaining)] [rest-nums (cdr remaining)])
          (if [< 0 f-num]
            (iter rest-nums (- left-sum f-num))
            (iter rest-nums left-sum))))

      (cond ([and [null? remaining] [= left-sum 0]] (add1 max-num))
            ([and [null? remaining] [not [= left-sum 0]]] left-sum)
            (else (continue))))

    (iter nums sum)))

(define test-nums-a '(1 2 0))
(define test-nums-b '(3 4 -1 1))

(first-missing-positive test-nums-a)
(first-missing-positive test-nums-b)
