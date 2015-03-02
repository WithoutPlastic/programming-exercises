#lang racket

;Problem:
;Given a binary tree, return the preorder traversal of its nodes' values.
;
;For example:
;Given binary tree {1,#,2,3},
;
;   1
;    \
;     2
;    /
;   3
;
;return [1,2,3].
;
;Note: Recursive solution is trivial, could you do it iteratively?

(require "lib/binary-tree.rkt")

(define [preorder-traversal root]
  (if [null? root] '()
    (cons (bnode-payload root)
          (append (preorder-traversal (bnode-left root))
                  (preorder-traversal (bnode-right root))))))

(define test-tree (btree-parse '(1 - 2 3)))
(preorder-traversal test-tree)
