#lang racket

;Problem:
;Given a binary tree, return the zigzag level order traversal of its nodes'
;values. (ie, from left to right, then right to left for the next level and
;alternate between).
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
;return its zigzag level order traversal as:
;
;[
;  [3],
;  [20,9],
;  [15,7]
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

(define [zigzag-level-order-traversal root]
  (define [btree->payload-levels root]
    (define [iter bnodes result]
      (if [null? bnodes] result
        (let* ([all-next-bnodes (append-map bnode->branches bnodes)]
               [next-bnodes (filter-not null? all-next-bnodes)]
               [exted-result (append result (list (map bnode-payload bnodes)))])
          (iter next-bnodes exted-result))))

    (iter (list root) '()))

  (letrec ([flat-l (λ [ll] (if [null? ll] ll
                               (append (car ll) (flat-r (cdr ll)))))]
           [flat-r (λ [ll] (if [null? ll] ll
                               (append (reverse (car ll)) (flat-l (cdr ll)))))])
    (flat-l (btree->payload-levels root))))

(zigzag-level-order-traversal (btree-parse '(3 9 20 - - 15 7)))
