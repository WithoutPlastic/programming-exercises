#lang racket

;Problem:
;Given a linked list, remove the nth node from the end of list and return its
;head.
;
;For example,
;
;Given linked list: 1->2->3->4->5, and n = 2.
;
;After removing the second node from the end, the linked list becomes
;1->2->3->5.
;
;Note: Given n will always be valid. Try to do this in one pass.

(require "lib/linked-node.rkt")

(define [remove-n-th-from-end! lnodes n]
  (define [n-lnode-next n] (apply compose (make-list n lnode-next)))

  (define [walk head forward-lnode]
    (if [lnode-last? forward-lnode]
      (lnode-set-next! head (lnode-next (lnode-next head)))
      (walk (lnode-next head) (lnode-next forward-lnode))))

  (walk lnodes ((n-lnode-next n) lnodes)))

(define test-linked-nodes (new-linked-nodes (range 1 6)))

(remove-n-th-from-end! test-linked-nodes 2)
(displayln test-linked-nodes)
