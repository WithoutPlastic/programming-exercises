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

(require "lib/linked-node.rkt")

(define [my-partition lnodes x]
  (let ([s-linked-list (make-linked-list '())]
        [l-linked-list (make-linked-list '())])
    (define [iter remaining s-tail l-tail]
      (define [join]
        (lnode-set-next! l-tail '())
        (lnode-set-next! s-tail (linked-list-body l-linked-list))
        s-linked-list)

      (if [null? remaining] (join)
        (let ([v (lnode-payload remaining)]
              [rest-r (lnode-next remaining)])
          (if [< v x]
            (begin (lnode-set-next! s-tail remaining)
                   (iter rest-r (lnode-next s-tail) l-tail))
            (begin (lnode-set-next! l-tail remaining)
                   (iter rest-r s-tail (lnode-next l-tail)))))))

    (iter lnodes s-linked-list l-linked-list)))

(define test-linked-nodes (new-linked-nodes '(1 4 3 2 5 2)))
(define test-target 3)

(my-partition test-linked-nodes test-target)
