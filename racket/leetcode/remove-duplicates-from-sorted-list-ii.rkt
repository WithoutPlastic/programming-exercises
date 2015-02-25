#lang racket

;Problem:
;Given a sorted linked list, delete all nodes that have duplicate numbers,
;leaving only distinct numbers from the original list.
;
;For example,
;Given 1->2->3->3->4->4->5, return 1->2->5.
;Given 1->1->1->2->3, return 2->3.

(require "lib/linked-node.rkt")

(define [delete-duplicates! linked-list]
  (define [iter head]
    (when [nor [null? head] [last-node? head] [last-node? (node-next head)]]
      (let* ([first-node (node-next head)]
             [f-val (node-payload first-node)]
             [second-node (node-next first-node)]
             [s-val (node-payload second-node)])
        (define [walk node]
          (define [continue]
            (let* ([next-node (node-next node)]
                   [next-val (node-payload next-node)])
              (if [eq? f-val next-val]
                (walk next-node)
                (begin (set-node-next! head next-node) (iter head)))))

          (if [last-node? node] (set-node-next! head '()) (continue)))

        (if [eq? f-val s-val] (walk second-node) (iter first-node)))))

  (iter linked-list))

(define test-linked-list-a (new-linked-list 1 2 3 3 4 4 5))
(define test-linked-list-b (new-linked-list 1 1 1 2 3))

(delete-duplicates! test-linked-list-a) (displayln test-linked-list-a)
(delete-duplicates! test-linked-list-b) (displayln test-linked-list-b)
