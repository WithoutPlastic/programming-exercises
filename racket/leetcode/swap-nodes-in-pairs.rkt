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

(define double-node-next (compose node-next node-next))
(define triple-node-next (compose node-next node-next node-next))

(define [swap-pairs linked-node]
  (define [iter head]
    (define [continue]
      (let ([next-one-node (node-next head)]
            [next-two-node (double-node-next head)]
            [next-three-node (triple-node-next head)])
        (set-node-next! head next-two-node)
        (set-node-next! next-two-node next-one-node)
        (set-node-next! next-one-node next-three-node)
        (iter next-one-node)))

    (when [and [not [last-node? head]] [not [last-node? (node-next head)]]]
      (continue)))

  (iter linked-node)
  linked-node)

(define test-linked-list-a (new-linked-list 1 2 3 4))
(define test-linked-list-b (new-linked-list 1 2 3 4 5))

(swap-pairs test-linked-list-a)
(swap-pairs test-linked-list-b)
