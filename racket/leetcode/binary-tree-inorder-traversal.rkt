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
      (traversal
        (bnode-left bnode) result
        (λ [ret]
           (traversal
             (bnode-right bnode) ret
             (λ [ret] (back (append ret (list (bnode-payload bnode))))))))))

  (traversal (bnode-left root) '()
             (λ [ret] (traversal (bnode-right root) ret
                                 (λ [ret] (cons (bnode-payload root) ret))))))

(define test-tree (btree-parse '(1 2 3 - - 4 - - 5)))

(inorder-traversal test-tree)
