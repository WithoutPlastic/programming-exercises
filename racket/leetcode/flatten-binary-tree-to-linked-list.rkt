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
  (define [iter tree-node tail-node back]
    (define [continue]
      (let* ([payload (btree-payload tree-node)]
             [new-linked-node (make-node payload '())])
        (set-node-next! tail-node new-linked-node)
        (iter (btree-left tree-node)
              new-linked-node
              (λ [tn] (iter (btree-right tree-node) tn back)))))

    (if [null? tree-node] (back tail-node) (continue)))

  (let ([linked-list (make-linked-list '())])
    (iter root linked-list (const '()))
    linked-list))

(define test-tree (btree-parse '(1 2 5 3 4 6)))

(tree-flatten test-tree)
