#lang racket

;Problem:
;Given an array S of n integers, find three integers in S such that the sum is
;closest to a given number, target. Return the sum of the three integers. You
;may assume that each input would have exactly one solution.
;
;For example, given array S = {-1 2 1 -4}, and target = 1.
;
;The sum that is closest to the target is 2. (-1 + 2 + 1 = 2).

(define [three-sum-closest nums sum]
  (define [find remainings target-sum]
    (define [find-with-one remainings m]
      (define [find-with-two remainings n]
        (let* ([validator (lambda [x] [and [= (+ m n x) target-sum] x])]
               [x [ormap validator remainings]])
          [and x (list m n x)]))

      (let ([first-num (car remainings)] [rest-nums (cdr remainings)])
        (if [< 1 (length remainings)]
          (or (find-with-two rest-nums first-num) (find-with-one rest-nums m))
          false)))

    (let ([first-num (car remainings)] [rest-nums (cdr remainings)])
      (if [< 2 (length remainings)]
        (or (find-with-one rest-nums first-num) (find rest-nums target-sum))
        false)))

  (let ([max-sum (* 3 (apply max nums))]
        [min-sum (* 3 (apply min nums))])
    (define [shift-iter shift]
      (let ([l-sum (+ sum shift)]
            [s-sum (- sum shift)])
        (if [and [< max-sum l-sum] [< s-sum min-sum]]
          false
          (or (find nums l-sum) (find nums s-sum)
              (shift-iter (add1 shift))))))

     (shift-iter 0)))

(define test-nums-a '(-1 2 1 -4))
(define test-target-a 1)
(define test-nums-b '(-1 2 1 -4 100))
(define test-target-b 90)

(three-sum-closest test-nums-b test-target-b)
