#lang racket

;Problem:
;Given preorder and inorder traversal of a tree, construct the binary tree.
;
;Note:
;You may assume that duplicates do not exist in the tree.

(require "lib/binary-tree.rkt")

(define [build-tree preorder inorder]
  (if [or [null? preorder] [null? inorder]] '()
    (let* ([root-payload (car preorder)]
           [not-eq-root-bnode? (negate (curry eq? root-payload))]
           [inorder-left (takef inorder not-eq-root-bnode?)]
           [inorder-right (cdr (dropf inorder not-eq-root-bnode?))]
           [preorder-left (take (cdr preorder) (length inorder-left))]
           [preorder-right (take-right preorder (length inorder-right))]
           [left-branch (build-tree preorder-left inorder-left)]
           [right-branch (build-tree preorder-right inorder-right)])
      (make-btree-node root-payload left-branch right-branch))))

(btree-serialize (build-tree '(f b a d c e g i h) '(a b c d e f g h i)))
