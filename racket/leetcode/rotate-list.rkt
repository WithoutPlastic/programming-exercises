#lang racket

;Problem:
;Given a list, rotate the list to the right by k places, where k is
;non-negative.
;
;For example:
;Given 1->2->3->4->5->NULL and k = 2,
;return 4->5->1->2->3->NULL.

(require "lib/linked-node.rkt")

(define [repeat-lnode-next n]
  (if [< 1 n] (compose lnode-next (repeat-lnode-next (sub1 n))) lnode-next))

(define [rotate-right head k]
  (let ([scout ((repeat-lnode-next k) head)])
    (define [iter h s]
      (define [rotate]
        (let ([first-node (lnode-next head)]
              [rotate-node (lnode-next h)])
          (set-lnode-next! head rotate-node)
          (set-lnode-next! s first-node)
          (set-lnode-next! h '())
          head))

      (if [null? (lnode-next s)] (rotate) (iter (lnode-next h) (lnode-next s))))

    (iter head scout)))

(define test-list (new-linked-list 1 2 3 4 5))

(rotate-right test-list 2)
