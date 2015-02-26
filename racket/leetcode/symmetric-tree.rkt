#lang racket

;Problem:
;Given a binary tree, check whether it is a mirror of itself
;(ie, symmetric around its center).
;
;For example, this binary tree is symmetric:
;
;     1
;    / \
;   2   2
;  / \ / \
; 3  4 4  3
;
;But the following is not:
;
;    1
;   / \
;  2   2
;   \   \
;   3    3
;
;Note:
;Bonus points if you could solve it both recursively and iteratively.
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

(define [symmetric-tree? root]
  (define [symmetric-equal? l-node r-node]
    (cond ([and [null? l-node] [null? r-node]] true)
          ([or [null? l-node] [null? r-node]] false)
          (else
            [and [equal? (btree-payload l-node) (btree-payload r-node)]
                 [symmetric-equal? (btree-left l-node) (btree-right r-node)]
                 [symmetric-equal? (btree-right l-node) (btree-left r-node)]])))

  [symmetric-equal? (btree-left root) (btree-right root)])

(define test-tree-a (btree-parse '(1 2 2 3 4 4 3)))
(define test-tree-b (btree-parse '(1 2 2 - 3 - 3)))

(symmetric-tree? test-tree-a)
(symmetric-tree? test-tree-b)
