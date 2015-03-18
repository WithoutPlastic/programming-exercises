#lang racket

;Problem:
;Rotate an array of n elements to the right by k steps.
;
;For example, with n = 7 and k = 3, the array [1,2,3,4,5,6,7] is rotated to
;[5,6,7,1,2,3,4].
;
;Note:
;Try to come up as many solutions as you can, there are at least 3 different
;ways to solve this problem.
;
;Hint:
;Could you do it in-place with O(1) extra space?
;
;Credits:
;Special thanks to @Freezen for adding this problem and creating all test cases.

(define [shift-left lst] (append (cdr lst) (list (car lst))))
(define [shift-right lst] (cons (last lst) (drop-right lst 1)))

(define [rotate.v1 lst k] (if [= 0 k] lst (rotate.v1 (shift-right lst) (sub1 k))))

(define [rotate.v2 lst k] (append (take-right lst k) (drop-right lst k)))

(define [rotate.v3 lst k]
  (if [<= k (floor (/ (length lst) 2))]
    (let ([mid-slice (reverse (drop lst k))])
      (append (reverse (take mid-slice k))
              (take lst k)
              (reverse (drop mid-slice k))))
    (reverse (rotate.v3 (reverse lst) (- (length lst) k)))))

(define [rotate.v4 lst k]
  (call-with-values (λ [] (split-at-right lst k))
                    (λ [h t] (reverse (append (reverse h) (reverse t))))))

(define test-lst (range 0 20))

(rotate.v1 test-lst 5)
(rotate.v1 test-lst 15)
(rotate.v2 test-lst 5)
(rotate.v2 test-lst 15)
(rotate.v3 test-lst 5)
(rotate.v3 test-lst 6)
(rotate.v4 test-lst 5)
(rotate.v4 test-lst 15)
