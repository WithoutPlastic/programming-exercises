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
  (define [find-paths node]
    (let* ([left-br (btree-left node)] [right-br (btree-right node)]
           [payload (btree-payload node)] [ext-payload (curry cons payload)])
      (cond ([btree-branches-non-empty? node]
             (append (map ext-payload (find-paths left-br))
                     (map ext-payload (find-paths right-br))))
            ([btree-last? node] (list (list payload)))
            ([btree-left-empty? node] (map ext-payload (find-paths right-br)))
            (else (map ext-payload (find-paths left-br))))))

  (if [null? root]
    false
    (filter (λ [p] [= sum (apply + p)]) (find-paths root))))

(define test-tree (btree-parse '(5 4 8 11 - 13 4 7 2 - - - 5 1)))

(get-paths-by-sum test-tree 22)
