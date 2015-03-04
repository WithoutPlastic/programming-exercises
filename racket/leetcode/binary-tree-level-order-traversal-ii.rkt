#lang racket

;Problem:
;Given a binary tree, return the bottom-up level order traversal of its nodes'
;values. (ie, from left to right, level by level from leaf to root).
;
;For example:
;Given binary tree {3,9,20,#,#,15,7},
;
;    3
;   / \
;  9  20
;    /  \
;   15   7
;
;return its bottom-up level order traversal as:
;
;[
;  [15,7],
;  [9,20],
;  [3]
;]
;
;confused what "{1,#,2,3}" means?
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

(define [bottom-level-order-traversal root]
  (define [iter-level bnodes]
    (let* ([cur-level-elts (map bnode-payload bnodes)]
           [next-level-bnodes (append-map bnode->branches bnodes)]
           [valid-next-level-bnodes (filter-not null? next-level-bnodes)])
      (if [null? valid-next-level-bnodes] (list cur-level-elts)
        (append (iter-level valid-next-level-bnodes) (list cur-level-elts)))))

  (iter-level (list root)))

(define test-tree (btree-parse '(3 9 20 - - 15 7)))

(bottom-level-order-traversal test-tree)
