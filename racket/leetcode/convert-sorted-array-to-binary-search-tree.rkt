#lang racket

;Problem:
;Given an array where elements are sorted in ascending order, convert it to a
;height balanced BST.

(require "lib/binary-tree.rkt")

(define sorted-list->bst list->btree)

(define test-list (range 0 24))

(btree-serialize (sorted-list->bst test-list))
