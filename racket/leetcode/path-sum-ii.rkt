#lang racket

;Problem:
;Given a binary tree and a sum, find all root-to-leaf paths where each path's
;sum equals the given sum.
;
;For example:
;Given the below binary tree and sum = 22,
;
;              5
;             / \
;            4   8
;           /   / \
;          11  13  4
;         /  \    / \
;        7    2  5   1
;
;return
;[
;   [5,4,11,2],
;   [5,8,4,5]
;]

(require "lib/binary-tree.rkt")

(define [get-paths-by-sum root sum]
  (if [null? root] '()
    (filter (λ [p] [= sum (apply + p)])
            (map (λ [p] (map bnode-payload p)) (btree-paths root)))))

(define test-tree (btree-parse '(5 4 8 11 - 13 4 7 2 - - 5 1)))

(get-paths-by-sum test-tree 22)
