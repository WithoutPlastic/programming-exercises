#lang racket

;Problem:
;Given a binary tree, return the postorder traversal of its nodes' values.
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
;return [3,2,1].
;
;Note: Recursive solution is trivial, could you do it iteratively?

(require "lib/binary-tree.rkt")

(define [postorder-traversal root]
  (if [null? root] '()
    (append (postorder-traversal (bnode-right root))
            (postorder-traversal (bnode-left root))
            (list (bnode-payload root)))))

(define test-tree (btree-parse '(1 - 2 3)))

(postorder-traversal test-tree)
