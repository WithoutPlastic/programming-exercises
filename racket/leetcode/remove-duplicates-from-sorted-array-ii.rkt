#lang racket

;Problem:
;Follow up for "Remove Duplicates":
;What if duplicates are allowed at most twice?
;
;For example,
;Given sorted array A = [1,1,1,2,2,3],
;
;Your function should return length = 5, and A is now [1,1,2,2,3].

(define [rm-duplicates! boxed-sorted-nums reserve]
  (define [iter stub remaining repeat accum cnt]
    (define [continue]
      (let ([first-r (car remaining)] [rest-rs (cdr remaining)])
        (if [< repeat reserve]
          (if [= first-r stub]
            (iter stub rest-rs (add1 repeat)
                  (append accum (list first-r)) (add1 cnt))
            (iter first-r rest-rs 1
                  (append accum (list first-r)) (add1 cnt)))
          (if [= first-r stub]
            (iter stub rest-rs (add1 repeat) accum cnt)
            (iter first-r rest-rs 1
                  (append accum (list first-r)) (add1 cnt))))))

    (if [null? remaining] (cons accum cnt) (continue)))

  (let* ([sorted-nums (unbox boxed-sorted-nums)]
         [first-num (car sorted-nums)] [rest-nums (cdr sorted-nums)]
         [result (iter first-num rest-nums 1 (list first-num) 1)])
    (set-box! boxed-sorted-nums (car result))
    (cdr result)))

(define test-sorted-nums-a (box '(1 1 1 2 2 3)))
(define test-sorted-nums-b (box '(0 0 0 0 0 1 2 3 4 5 6 7 7 8 9)))

(rm-duplicates! test-sorted-nums-a 2)
test-sorted-nums-a
(rm-duplicates! test-sorted-nums-b 2)
test-sorted-nums-b
