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
  (define [find-paths bnode]
    (let* ([left-bnode (bnode-left bnode)] [right-bnode (bnode-right bnode)]
           [payload (bnode-payload bnode)] [ext-payload (curry cons payload)])
      (cond ([bnode-branches-non-empty? bnode]
             (append (map ext-payload (find-paths left-bnode))
                     (map ext-payload (find-paths right-bnode))))
            ([bnode-branches-empty? bnode] (list (list payload)))
            ([bnode-left-empty? bnode]
             (map ext-payload (find-paths right-bnode)))
            (else (map ext-payload (find-paths left-bnode))))))

  (if [null? root] '()
    (filter (λ [p] [= sum (apply + p)]) (find-paths root))))

(define test-tree (btree-parse '(5 4 8 11 - 13 4 7 2 - - 5 1)))

(get-paths-by-sum test-tree 22)
