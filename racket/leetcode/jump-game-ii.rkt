#lang racket

;Problem:
;Given an array of non-negative integers, you are initially positioned at the
;first index of the array.
;
;Each element in the array represents your maximum jump length at that position.
;
;Your goal is to reach the last index in the minimum number of jumps.
;
;For example:
;Given array A = [2,3,1,1,4]
;
;The minimum number of jumps to reach the last index is 2. (Jump 1 step from
;index 0 to 1, then 3 steps to the last index.)

(define [jump ints]
  (let ([len (length ints)])
    (define [iter froms cnt]
      (let* ([idx-valid? (lambda [idx] [and [<= 0 idx] [< idx len]])]
             [low-bdy (lambda [idx] (- idx (list-ref ints idx)))]
             [high-bdy (lambda [idx] (add1 (+ idx (list-ref ints idx))))]
             [next-range (lambda [idx] (range (low-bdy idx) (high-bdy idx)))]
             [reach-range (filter idx-valid? (append-map next-range froms))])
        (if [memq (sub1 len) reach-range] cnt (iter reach-range (add1 cnt)))))

    (iter '(0) 1)))

(define test-input '(2 3 1 1 4))

(jump test-input)
