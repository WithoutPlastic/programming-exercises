#lang racket

;Problem:
;Given a binary tree where all the right nodes are either leaf nodes with a
;sibling (a left node that shares the same parent node) or empty, flip it
;upside down and turn it into a tree where the original right nodes turned into
;left leaf nodes. Return the new root.
;
;For example:
;Given a binary tree {1,2,3,4,5},
;
;      1
;     / \
;    2   3
;   / \
;  4   5
;
;return the root of the binary tree [4,5,2,#,#,3,1].
;
;      4
;     / \
;    5   2
;       / \
;      3   1

(require "lib/binary-tree.rkt")

(define [upside-down-binary-tree! root]
  (define [iter bnode parent-bnode twin-bnode]
    (if [null? bnode] parent-bnode
      (let ([next-parent bnode]
            [next-twin (bnode-right bnode)]
            [next-bnode (bnode-left bnode)])
        (bnode-set-left! bnode twin-bnode)
        (bnode-set-right! bnode parent-bnode)
        (iter next-bnode next-parent next-twin))))

  (iter root '() '()))

(define test-tree (btree-parse '(1 2 3 4 5)))

(btree-serialize (upside-down-binary-tree! test-tree))
