#lang racket

;Problem:
;Given a binary tree, determine if it is height-balanced.
;
;For this problem, a height-balanced binary tree is defined as a binary tree in
;which the depth of the two subtrees of every node never differ by more than 1.

(require "lib/binary-tree.rkt")

(define [balanced? root]
  (let ([left-br (bnode-left root)] [right-br (bnode-right root)])
    (cond ([bnode-last? root] true)
          ([bnode-branches-non-empty? root]
           [and [<= (- (btree-height left-br) (btree-height right-br)) 1]
                [balanced? left-br] [balanced? right-br]])
          ([bnode-left-empty? root] [<= (btree-height right-br) 1])
          (else [<= (btree-height left-br) 1]))))

(define test-tree (btree-parse '(a b e c d f - - - - - g)))

(balanced? test-tree)
