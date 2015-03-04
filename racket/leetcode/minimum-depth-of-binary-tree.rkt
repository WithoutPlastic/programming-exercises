#lang racket

;Problem:
;Given a binary tree, find its minimum depth.
;
;The minimum depth is the number of nodes along the shortest path from the root
;node down to the nearest leaf node.

(require "lib/binary-tree.rkt")

(define [min-depth root]
  (cond ([null? root] 0)
        ([bnode-branches-non-empty? root]
         (add1 (min (min-depth (bnode-left root))
                    (min-depth (bnode-right root)))))
        (else 1)))

(define test-tree (btree-parse '(a b e c d f - - - - - g - h)))

(min-depth test-tree)
