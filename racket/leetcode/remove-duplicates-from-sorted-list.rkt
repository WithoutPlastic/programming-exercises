#lang racket

;Problem:
;Given a sorted linked list, delete all duplicates such that each element
;appear only once.
;
;For example,
;Given 1->1->2, return 1->2.
;Given 1->1->2->3->3, return 1->2->3.

(require "linked-node.rkt")

(define [delete-duplicates! linked-list]
  (define [walk backward-node forward-node]
    (define [continue]
      (let ([f-val (node-payload forward-node)]
            [b-val (node-payload backward-node)]
            [next-node (node-next forward-node)])
        (if [eq? f-val b-val]
          (begin (set-node-next! backward-node next-node)
                 (walk backward-node next-node))
          (walk forward-node next-node))))

    (unless [null? forward-node] (continue)))

  (let ([first-node (node-next linked-list)])
    (when [nor [last-node? linked-list] [last-node? first-node]]
      (walk linked-list first-node))))

(define test-linked-list-a (new-linked-list 1 1 2))
(define test-linked-list-b (new-linked-list 1 1 2 3 3))

(delete-duplicates! test-linked-list-a) (displayln test-linked-list-a)
(delete-duplicates! test-linked-list-b) (displayln test-linked-list-b)
