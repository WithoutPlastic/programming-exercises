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
  (define [traversal node result back]
    (if [null? node]
      (back result)
      (traversal
        (btree-left node) result
        (λ [ret]
           (traversal
             (btree-right node) ret
             (λ [ret] (back (append ret (list (btree-payload node))))))))))

  (traversal (btree-left root) '()
             (λ [ret] (traversal (btree-right root) ret
                                 (λ [ret] (cons (btree-payload root) ret))))))

(define test-tree (btree-parse '(1 2 3 - - 4 - - 5)))

(inorder-traversal test-tree)
