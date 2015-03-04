#lang racket

;Problem:
;Given an array of non-negative integers, you are initially positioned at the
;first index of the array.
;
;Each element in the array represents your maximum jump length at that position.
;
;Determine if you are able to reach the last index.
;
;For example:
;
;A = [2,3,1,1,4], return true.
;A = [3,2,1,0,4], return false.

(define [can-jump? ints]
  (let* ([len (length ints)]
         [limits (append (range (- (sub1 len) 1) -1 -1) '(+inf.0))])
    (ormap < limits ints)))

(define test-a '(2 3 1 1 4))
(define test-b '(3 2 1 0 4))

(can-jump? test-a)
(can-jump? test-b)
