#lang racket

;Problem:
;Given inorder and postorder traversal of a tree, construct the binary tree.
;
;Note:
;You may assume that duplicates do not exist in the tree.

(require "lib/binary-tree.rkt")

(define [build-tree inorder postorder]
  (if [or [null? postorder] [null? inorder]] '()
    (let* ([root-payload (last postorder)]
           [not-eq-root-bnode? (negate (curry eq? root-payload))]
           [inorder-left (takef inorder not-eq-root-bnode?)]
           [inorder-right (cdr (dropf inorder not-eq-root-bnode?))]
           [left-len (length inorder-left)]
           [right-len (length inorder-right)]
           [postorder-left (take postorder left-len)]
           [postorder-right (take (drop postorder left-len) right-len)]
           [left-branch (build-tree inorder-left postorder-left)]
           [right-branch (build-tree inorder-right postorder-right)])
      (make-btree-node root-payload left-branch right-branch))))

(btree-serialize (build-tree '(a b c d e f g h i) '(a c e d b h i g f)))
