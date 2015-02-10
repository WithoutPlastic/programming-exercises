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

;Utils
(define node-payload mcar)
(define node-next mcdr)
(define [set-node-payload! node payload] (set-mcar! node payload))
(define [set-node-next! node next] (set-mcdr! node next))
(define [make-node payload next] (mcons payload next))
(define [last-node? node] [null? (node-next node)])
(define [make-linked-list node] (mcons 'linked-list node))
(define linked-list-body mcdr)

(define [n-node-next node n]
  (if [< 0 n] (n-node-next (node-next node) (sub1 n)) node))

(define [remove-n-th-from-end! linked-list n]
  (let* ([body (linked-list-body linked-list)]
         [scout-node (n-node-next body n)])
    (define [iter cur-node scout-node]
      (if [last-node? scout-node]
        (set-node-next! cur-node (node-next (node-next cur-node)))
        (iter (node-next cur-node) (node-next scout-node))))

    (iter body scout-node)))

(define test-linked-list
  (make-linked-list (make-node 1
                    (make-node 2
                    (make-node 3
                    (make-node 4
                    (make-node 5 '())))))))

(displayln test-linked-list)
(remove-n-th-from-end! test-linked-list 2)
(displayln test-linked-list)
