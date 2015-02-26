#lang racket

;Problem
;Given a binary tree and a sum, determine if the tree has a root-to-leaf path
;such that adding up all the values along the path equals the given sum.
;
;For example:
;Given the below binary tree and sum = 22,
;
;              5
;             / \
;            4   8
;           /   / \
;          11  13  4
;         /  \      \
;        7    2      1
;
;return true, as there exist a root-to-leaf path 5->4->11->2 which sum is 22.

(require "lib/binary-tree.rkt")

(define [has-path-sum? root sum]
  (define [find-paths node]
    (let* ([left-br (bnode-left node)] [right-br (bnode-right node)]
           [payload (bnode-payload node)] [ext-payload (curry cons payload)])
      (cond ([bnode-branches-non-empty? node]
             (append (map ext-payload (find-paths left-br))
                     (map ext-payload (find-paths right-br))))
            ([bnode-last? node] (list (list payload)))
            ([bnode-left-empty? node] (map ext-payload (find-paths right-br)))
            (else (map ext-payload (find-paths left-br))))))

  (if [null? root]
    false
    [list? (memq sum (map (curry apply +) (find-paths root)))]))

(define test-tree (btree-parse '(5 4 8 11 - 13 4 7 2 - - - 1)))

(has-path-sum? test-tree 22)
