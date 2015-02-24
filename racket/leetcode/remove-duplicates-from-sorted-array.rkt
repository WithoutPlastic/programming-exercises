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

(define [rm-duplicates! boxed-sorted-nums]
  (define [iter stub remaining accum cnt]
    (define [continue]
      (let ([first-r (car remaining)] [rest-rs (cdr remaining)])
        (if [= stub first-r]
          (iter stub rest-rs accum cnt)
          (iter first-r rest-rs (append accum (list first-r)) (add1 cnt)))))

    (if [null? remaining] (cons accum cnt) (continue)))
  
  (let* ([sorted-nums (unbox boxed-sorted-nums)]
         [first-num (car sorted-nums)] [rest-nums (cdr sorted-nums)]
         [result (iter first-num rest-nums (list first-num) 1)])
    (set-box! boxed-sorted-nums (car result))
    (cdr result)))

(define test-sorted-nums (box '(0 0 0 0 0 1 2 3 4 5 6 7 7 8 9)))

(rm-duplicates! test-sorted-nums)
test-sorted-nums
