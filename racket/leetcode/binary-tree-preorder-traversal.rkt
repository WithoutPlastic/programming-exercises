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
  (define [iter bnode result back]
    (if [null? bnode] (back result)
      (iter
        (bnode-right bnode)
        result
        (λ [ret] (iter (bnode-left bnode)
                       ret
                       (λ [ret] (back (cons (bnode-payload bnode) ret))))))))

  (iter root '() identity))

(define test-tree (btree-parse '(f b g a d - i - - c e h)))
(preorder-traversal test-tree)
