#lang racket

;Problem:
;Given a binary tree, determine if it is a valid binary search tree (BST).
;
;Assume a BST is defined as follows:
;
;- The left subtree of a node contains only nodes with keys less than the node's
;  key.
;- The right subtree of a node contains only nodes with keys greater than the
;  node's key.
;- Both the left and right subtrees must also be binary search trees.
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

(define [valid-bst? root]
  (define [scan bnode verdict?]
    (if [null? bnode] true
      (let ([payload (bnode-payload bnode)]
            [left-bnode (bnode-left bnode)]
            [right-bnode (bnode-right bnode)])
        [and [verdict? payload]
             [scan left-bnode verdict?]
             [scan right-bnode verdict?]])))

  (if [null? root] true
    (let ([payload (bnode-payload root)]
          [left-bnode (bnode-left root)]
          [right-bnode (bnode-right root)])
      [and [scan left-bnode (curryr < payload)]
           [scan right-bnode (curry < payload)]
           [valid-bst? left-bnode]
           [valid-bst? right-bnode]])))

(define test-bst-a (btree-parse (range 1 12)))
(define test-bst-b (btree-parse '(6 3 7 1 4 - 8)))

(valid-bst? test-bst-a)
(valid-bst? test-bst-b)
