#lang racket

;Problem:
;Suppose a sorted array is rotated at some pivot unknown to you beforehand.
;
;(i.e., 0 1 2 4 5 6 7 might become 4 5 6 7 0 1 2).
;
;You are given a target value to search. If found in the array return its index,
;otherwise return -1.
;
;You may assume no duplicate exists in the array.

(define [search rs-nums target]
  (define [iter rs-nums offset]
    (let ([len (length rs-nums)])
      (define [continue]
        (let* ([ln (first rs-nums)]
               [rn (last rs-nums)]
               [mid-idx (floor (/ len 2))]
               [mn (list-ref rs-nums mid-idx)]
               [lp (take (drop rs-nums 1) (sub1 mid-idx))]
               [rp (take (drop rs-nums (add1 mid-idx)) (- len mid-idx 2))])
          (cond ([= target ln] offset)
                ([= target rn] (+ offset (sub1 len)))
                ([= target mn] (+ offset mid-idx))
                ([and [< ln mn] [< target mn] [< ln target]]
                 (iter lp (add1 offset)))
                ([< ln mn]
                 (iter rp (+ offset (add1 mid-idx))))
                ([and [< mn ln] [< mn target] [< target rn]]
                 (iter rp (+ offset (add1 mid-idx))))
                (else (iter lp (add1 offset))))))

      (cond ([null? rs-nums] -1)
            ([< 2 len] (continue))
            ([= (car rs-nums) target] offset)
            ([and [pair? (cdr rs-nums)] [= (cadr rs-nums) target]]
             (add1 offset))
            (else -1))))

  (iter rs-nums 0))

(define test-nums-a '(1 2 3 4 5 6 7 -4 -3 -2 -1 0))
(define test-target-a 3)

(search test-nums-a 3)
