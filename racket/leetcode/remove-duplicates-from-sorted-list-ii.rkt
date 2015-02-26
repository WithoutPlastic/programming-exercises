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
  (define [walk val lnode]
    (if [lnode-last? lnode] '()
      (let* ([next-lnode (lnode-next lnode)]
             [next-val (lnode-payload next-lnode)])
        (if [eq? val next-val]
          (walk val next-lnode)
          (iter next-lnode)))))

  (define [iter remaining]
    (if [lnode-last? remaining] remaining
      (let* ([first-lnode remaining]
             [first-val (lnode-payload first-lnode)]
             [second-lnode (lnode-next first-lnode)]
             [second-val (lnode-payload second-lnode)])
        (if [eq? first-val second-val]
          (walk first-val second-lnode)
          (begin (lnode-set-next! first-lnode (iter second-lnode))
                 first-lnode)))))

  (unless [linked-list-empty? linked-list]
    (linked-list-set-body! linked-list (iter (linked-list-body linked-list)))))

(define test-linked-list-a (new-linked-list 1 2 3 3 4 4 5))
(define test-linked-list-b (new-linked-list 1 1 1 2 3))

(delete-duplicates! test-linked-list-a) (displayln test-linked-list-a)
(delete-duplicates! test-linked-list-b) (displayln test-linked-list-b)
