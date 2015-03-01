#lang racket

;Problem:
;Write a program to find the node at which the intersection of two singly
;linked lists begins.
;
;For example, the following two linked lists:
;
;A:           a1 -> a2
;                     \
;                      c1 -> c2 -> c3
;                     /
;B:     b1 -> b2 -> b3
;
;begin to intersect at node c1.
;
;Notes:
;
; - If the two linked lists have no intersection at all, return null.
; - The linked lists must retain their original structure after the function returns.
; - You may assume there are no cycles anywhere in the entire linked structure.
; - Your code should preferably run in O(n) time and use only O(1) memory.
;
;Credits:
;Special thanks to @stellari for adding this problem and creating all test cases.

(require "lib/linked-node.rkt")

(define [get-intersection-node ll-a ll-b]
  (define [repeat f n] (foldl compose identity (make-list n f)))
  (define [iter a b] (if [eq? a b] a (iter (lnode-next a) (lnode-next b))))

  (let ([len-a (linked-list-length ll-a)] [len-b (linked-list-length ll-b)])
    (if [< len-a len-b]
      (iter ll-a ((repeat lnode-next (- len-b len-a)) ll-b))
      (iter ll-b ((repeat lnode-next (- len-a len-b)) ll-a)))))

(define shared-linked-list (new-linked-list 'a 'b 'c 'd))
(define test-linked-list-a
  (let ([new-linked-list (new-linked-list 1 2 3 4 5 6 7 8)])
    (linked-list-append! new-linked-list shared-linked-list)
    new-linked-list))
(define test-linked-list-b
  (let ([new-linked-list (new-linked-list 1 2 3 4)])
    (linked-list-append! new-linked-list shared-linked-list)
    new-linked-list))

(get-intersection-node test-linked-list-a test-linked-list-b)
