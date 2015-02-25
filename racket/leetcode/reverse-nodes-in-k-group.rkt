#lang racket

;Problem:
;Given a linked list, reverse the nodes of a linked list k at a time and return
;its modified list.
;
;If the number of nodes is not a multiple of k then left-out nodes in the end
;should remain as it is.
;
;You may not alter the values in the nodes, only nodes itself may be changed.
;
;Only constant memory is allowed.
;
;For example,
;Given this linked list: 1->2->3->4->5
;
;For k = 2, you should return: 2->1->4->3->5
;
;For k = 3, you should return: 3->2->1->4->5

(require "lib/linked-node.rkt")

(define [create-n-node-next n] (apply compose (build-list n (const node-next))))
(define [n-remaining? head n]
  (if [< 0 n]
    [and [not [last-node? head]]
         (n-remaining? (node-next head) (sub1 n))]
    true))

(define [reverse-k-group linked-nodes k]
  (define [reverse-section head tail]
    (let* ([node-by-offset (lambda [offset] ((create-n-node-next offset) head))]
           [elts (map node-by-offset (range 1 (add1 k)))]
           [hr-elts (cons head (reverse elts))]
           [lr-elts (reverse (cons tail elts))])
      (for-each set-node-next! hr-elts lr-elts)))

  (define [iter head]
    (when [n-remaining? head k]
      (reverse-section head ((create-n-node-next (add1 k)) head))
      (iter ((create-n-node-next k) head))))

  (when [< 1 k] (iter linked-nodes))
  linked-nodes)

(define test-linked-list (new-linked-list 1 2 3 4 5 6 7 8 9 10))

;(reverse-k-group test-linked-list 2)
(reverse-k-group test-linked-list 4)
