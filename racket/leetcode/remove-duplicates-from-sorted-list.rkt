#lang racket

;Problem:
;Given a sorted linked list, delete all duplicates such that each element
;appear only once.
;
;For example,
;Given 1->1->2, return 1->2.
;Given 1->1->2->3->3, return 1->2->3.

(require "lib/linked-node.rkt")

(define [delete-duplicates! linked-list]
  (define [iter lnode]
    (unless [lnode-last? lnode]
      (let* ([first-val (lnode-payload lnode)]
             [next-lnode (lnode-next lnode)]
             [next-val (lnode-payload next-lnode)])
        (if [eq? first-val next-val]
          (begin (lnode-set-next! lnode (lnode-next next-lnode))
                 (iter lnode))
          (iter next-lnode)))))

  (unless [linked-list-empty? linked-list]
    (iter (linked-list-body linked-list))))

(define test-linked-list-a (new-linked-list 1 1 2))
(define test-linked-list-b (new-linked-list 1 1 2 3 3))

(delete-duplicates! test-linked-list-a) (displayln test-linked-list-a)
(delete-duplicates! test-linked-list-b) (displayln test-linked-list-b)
