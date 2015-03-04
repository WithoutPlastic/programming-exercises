#lang racket

;Problem:
;Given two binary trees, write a function to check if they are equal or not.
;
;Two binary trees are considered equal if they are structurally identical and
;the nodes have the same value.

(require "lib/binary-tree.rkt")

(define same-tree? btree-equal?)

(define test-tree-a (btree-parse '(5 4 8 11 - 13 4 7 2 - - 5 1)))
(define test-tree-b (btree-parse '(5 4 8 11 - 13 4 7 2 - - 5 0)))

(same-tree? test-tree-a test-tree-a)
(same-tree? test-tree-a test-tree-b)
