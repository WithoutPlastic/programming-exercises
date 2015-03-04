#lang racket

;Problem:
;Given a binary tree, return the inorder traversal of its nodes' values.
;
;For example:
;Given binary tree {1,#,2,3},
;
; 1
;  \
;   2
;  /
; 3
;
;return [1,3,2].
;
;Note: Recursive solution is trivial, could you do it iteratively?
;
;confused what "{1,#,2,3}" means?
;
;> read more on how binary tree is serialized on OJ.
;
;OJ's Binary Tree Serialization:
;
;The serialization of a binary tree follows a level order traversal, where '#'
;signifies a path terminator where no node exists below.
;
;Here's an example:
;
;   1
;  / \
; 2   3
;    /
;   4
;    \
;     5
;
;The above binary tree is serialized as "{1,2,3,#,#,4,#,#,5}".

(require "lib/binary-tree.rkt")

(define [inorder-traversal root]
  (define [traversal bnode result back]
    (if [null? bnode] (back result)
      (traversal (bnode-right bnode)
                 result
                 (λ [ret] (traversal (bnode-left bnode)
                                     (cons (bnode-payload bnode) ret)
                                     (λ [ret] (back ret)))))))

  (traversal root '() identity))

(define test-tree (btree-parse '(f b g a d - i - - c e h)))

(inorder-traversal test-tree)
