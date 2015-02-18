#lang racket

;Problem:
;Given a linked list and a value x, partition it such that all nodes less than x
;come before nodes greater than or equal to x.
;
;You should preserve the original relative order of the nodes in each of the two
;partitions.
;
;For example,
;Given 1->4->3->2->5->2 and x = 3,
;return 1->2->2->4->3->5.

(require "linked-node.rkt")

(define [my-partition linked-list x]
  (let ([head (node-next linked-list)]
        [s-linked-list (make-linked-list '())]
        [l-linked-list (make-linked-list '())])
    (define [iter remaining s-cur-node l-cur-node]
      (define [continue]
        (let ([cur-n (node-payload remaining)]
              [rest-r (node-next remaining)])
          (if [< cur-n x]
            (begin (set-node-next! s-cur-node remaining)
                   (iter rest-r (node-next s-cur-node) l-cur-node))
            (begin (set-node-next! l-cur-node remaining)
                   (iter rest-r s-cur-node (node-next l-cur-node))))))

      (if [null? remaining]
        (begin (set-node-next! l-cur-node '())
               (set-node-next! s-cur-node (node-next l-linked-list))
               s-linked-list)
        (continue)))

    (iter head s-linked-list l-linked-list)))

(define test-linked-list (new-linked-list 1 4 3 2 5 2))
(define test-target 3)

(my-partition test-linked-list test-target)
