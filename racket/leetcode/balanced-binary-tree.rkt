#lang racket

;Problem:
;Given a binary tree, determine if it is height-balanced.
;
;For this problem, a height-balanced binary tree is defined as a binary tree in
;which the depth of the two subtrees of every node never differ by more than 1.

(require "lib/binary-tree.rkt")

(define [balanced? root]
  (let ([left (btree-left root)] [right (btree-right root)])
    (cond ([btree-last? root] true)
          ([btree-branches-non-empty? root]
           [and [<= (- (btree-height left) (btree-height right)) 1]
                [balanced? left] [balanced? right]])
          ([btree-left-empty? root] [<= (btree-height right) 1])
          (else [<= (btree-height left) 1]))))

(define test-tree (btree-parse '(a b e c d f - - - - - g)))
(balanced? test-tree)
