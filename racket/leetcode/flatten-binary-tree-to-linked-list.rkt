#lang racket

;Problem:
;Given a binary tree, flatten it to a linked list in-place.
;
;For example,
;Given
;
;         1
;        / \
;       2   5
;      / \   \
;     3   4   6
;
;The flattened tree should look like:
;
;   1
;    \
;     2
;      \
;       3
;        \
;         4
;          \
;           5
;            \
;             6
;
;
;Hints:
;If you notice carefully in the flattened tree, each node's right child points
;to the next node of a pre-order traversal.

(require "lib/linked-node.rkt")
(require "lib/binary-tree.rkt")

(define [tree-flatten root]
  (define [iter bnode tail-lnode back]
    (if [null? bnode] (back tail-lnode)
      (let ([new-lnode (make-lnode (bnode-payload bnode) '())])
        (lnode-set-next! tail-lnode new-lnode)
        (iter (bnode-left bnode) new-lnode
              (λ [tn] (iter (bnode-right bnode) tn back))))))

  (let ([linked-list (make-linked-list '())])
    (iter root linked-list (λ _ _))
    linked-list))

(define test-tree (btree-parse '(1 2 5 3 4 6)))

(tree-flatten test-tree)
