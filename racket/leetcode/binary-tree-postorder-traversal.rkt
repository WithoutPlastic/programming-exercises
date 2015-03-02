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
  (define [iter bnode result back]
    (if [null? bnode] (back result)
      (iter (bnode-right bnode)
            (cons (bnode-payload bnode) result)
            (λ [ret] (iter (bnode-left bnode) ret (λ [ret] (back ret)))))))

  (iter root '() identity))

(define test-tree (btree-parse '(1 - 2 3)))

(postorder-traversal test-tree)
