#lang racket

;Problem:
;Given an array S of n integers, are there elements a, b, c, and d in S such
;that a + b + c + d = target? Find all unique quadruplets in the array which
;gives the sum of target.
;
;Note:
;Elements in a quadruplet (a,b,c,d) must be in non-descending order.
;(ie, a ≤ b ≤ c ≤ d)

;The solution set must not contain duplicate quadruplets.
;For example, given array S = {1 0 -1 0 -2 2}, and target = 0.
;
;A solution set is:
;(-1,  0, 0, 1)
;(-2, -1, 1, 2)
;(-2,  0, 0, 2)

(define [find-four-sum nums sum]
  (let ([sorted-nums (sort nums <)]
        [len (length nums)])
    (define [find-with-one ll-idx]
      (define [find-with-two hh-idx]
        (define [find-with-three l-idx]
          (define [find-with-four h-idx]
            (define [ok?]
              (let ([ll-num (list-ref sorted-nums ll-idx)]
                    [hh-num (list-ref sorted-nums hh-idx)]
                    [l-num (list-ref sorted-nums l-idx)]
                    [h-num (list-ref sorted-nums h-idx)])
                (if [= (+ ll-num l-num h-num hh-num) sum]
                  (list (list ll-num l-num h-num hh-num)) '())))

            (if [< l-idx h-idx]
              (append (ok?) (find-with-four (sub1 h-idx)))
              '()))

          (if [< (add1 l-idx) hh-idx]
            (append (find-with-four (sub1 hh-idx))
                    (find-with-three (add1 l-idx)))
            '()))
        
        (if [< (add1 ll-idx) (sub1 hh-idx)]
          (append (find-with-three (add1 ll-idx))
                  (find-with-two (sub1 hh-idx)))
          '()))

      (if [< (+ ll-idx 3) len]
        (append (find-with-two (sub1 len))
                (find-with-one (add1 ll-idx)))
        '()))

    (find-with-one 0)))

(define test-nums-a '(1 0 -1 0 -2 2))
(define test-sum-a 0)

(find-four-sum test-nums-a test-sum-a)
