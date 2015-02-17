#lang racket

;Problem:
;Given a list, rotate the list to the right by k places, where k is
;non-negative.
;
;For example:
;Given 1->2->3->4->5->NULL and k = 2,
;return 4->5->1->2->3->NULL.

(require "linked-node.rkt")

(define [repeat-node-next n]
  (if [< 1 n] (compose node-next (repeat-node-next (sub1 n))) node-next))

(define [rotate-right head k]
  (let ([scout ((repeat-node-next k) head)])
    (define [iter h s]
      (define [rotate]
        (let ([first-node (node-next head)]
              [rotate-node (node-next h)])
          (set-node-next! head rotate-node)
          (set-node-next! s first-node)
          (set-node-next! h '())
          head))

      (if [null? (node-next s)] (rotate) (iter (node-next h) (node-next s))))

    (iter head scout)))

(define test-list
  (make-linked-list
    (make-node 1 (make-node 2 (make-node 3 (make-node 4 (make-node 5 '())))))))

(rotate-right test-list 2)
