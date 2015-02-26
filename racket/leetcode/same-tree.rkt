#lang racket

;Problem:
;Given two binary trees, write a function to check if they are equal or not.
;
;Two binary trees are considered equal if they are structurally identical and
;the nodes have the same value.

(require "lib/binary-tree.rkt")

(define [same-tree? root-a root-b]
  (define [continue]
    (let ([a-left-br (btree-left root-a)] [a-right-br (btree-right root-a)]
          [b-left-br (btree-left root-b)] [b-right-br (btree-right root-b)]
          [payload-a (btree-payload root-a)] [payload-b (btree-payload root-b)])
      [and [equal? payload-a payload-b]
           [same-tree? a-left-br b-left-br]
           [same-tree? a-right-br b-right-br]]))

  (cond ([and [null? root-a] [null? root-b]] true)
        ([or [null? root-a] [null? root-b]] false)
        (else (continue))))

(define test-tree-a (btree-parse '(5 4 8 11 - 13 4 7 2 - - 5 1)))
(define test-tree-b (btree-parse '(5 4 8 11 - 13 4 7 2 - - 5 0)))

(same-tree? test-tree-a test-tree-a)
(same-tree? test-tree-a test-tree-b)
