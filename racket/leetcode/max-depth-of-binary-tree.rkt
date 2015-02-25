#lang racket

;Problem
;Given a binary tree, find its maximum depth.
;
;The maximum depth is the number of nodes along the longest path from the root
;node down to the farthest leaf node.

(require "lib/binary-tree.rkt")

(define max-depth btree-height)

(define test-tree (btree-parse '(a b e c d f - - - - - g - h)))

(max-depth test-tree)
