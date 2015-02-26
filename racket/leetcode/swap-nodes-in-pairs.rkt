#lang racket

;Problem:
;Given a linked list, swap every two adjacent nodes and return its head.
;
;For example,
;Given 1->2->3->4, you should return the list as 2->1->4->3.
;
;Your algorithm should use only constant space. You may not modify the values in
;the list, only nodes itself can be changed.

(require "lib/linked-node.rkt")

(define double-lnode-next (compose lnode-next lnode-next))
(define triple-lnode-next (compose lnode-next lnode-next lnode-next))

(define [swap-pairs linked-node]
  (define [iter head]
    (define [continue]
      (let ([next-one-node (lnode-next head)]
            [next-two-node (double-lnode-next head)]
            [next-three-node (triple-lnode-next head)])
        (set-lnode-next! head next-two-node)
        (set-lnode-next! next-two-node next-one-node)
        (set-lnode-next! next-one-node next-three-node)
        (iter next-one-node)))

    (when [and [not [lnode-last? head]] [not [lnode-last? (lnode-next head)]]]
      (continue)))

  (iter linked-node)
  linked-node)

(define test-linked-list-a (new-linked-list 1 2 3 4))
(define test-linked-list-b (new-linked-list 1 2 3 4 5))

(swap-pairs test-linked-list-a)
(swap-pairs test-linked-list-b)
