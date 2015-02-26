#lang racket

;Problem:
;Merge two sorted linked lists and return it as a new list. The new list should
;be made by splicing together the nodes of the first two list.

(require "lib/linked-node.rkt")

(define [merge-two-sorted-lists lnodes-a lnodes-b]
  (define [continue]
    (if [<= (lnode-payload lnodes-a) (lnode-payload lnodes-b)]
      (let ([left-ret (merge-two-sorted-lists (lnode-next lnodes-a) lnodes-b)])
        (lnode-set-next!  lnodes-a left-ret) lnodes-a)
      (merge-two-sorted-lists lnodes-b lnodes-a)))

  (cond ([lnode-last? lnodes-a] lnodes-b)
        ([lnode-last? lnodes-b] lnodes-a)
        (else (continue))))

(define test-a (new-linked-nodes -10 -8 -4 -1 0 0 1 7 9))
(define test-b (new-linked-nodes -9 -7 -4 -1 1 2 4 6 6))

(merge-two-sorted-lists test-a test-b)
