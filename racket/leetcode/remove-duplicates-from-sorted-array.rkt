#lang racket

;Problem:
;Given a sorted array, remove the duplicates in place such that each element
;appear only once and return the new length.
;
;Do not allocate extra space for another array, you must do this in place with
;constant memory.
;
;For example,
;Given input array A = [1,1,2],
;
;Your function should return length = 2, and A is now [1,2].

(define [rm-duplicates sorted-nums]
  (define [iter n remainings cnt]
    (define [continue]
      (let ([next-num (car remainings)]
            [nexts (cdr remainings)])
        (if [= n next-num]
          (iter n nexts cnt)
          (cons n (iter next-num nexts (add1 cnt))))))

    (if [null? remainings] (list n) (continue)))
  
  (set! test-sorted-nums (iter (car sorted-nums) (cdr sorted-nums) 1))
  (length test-sorted-nums))

(define test-sorted-nums '(0 0 0 0 0 1 2 3 4 5 6 7 7 8 9))

(rm-duplicates test-sorted-nums)
test-sorted-nums
