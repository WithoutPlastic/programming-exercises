#lang racket

;Problem:
;Given a binary tree, find the maximum path sum.
;
;The path may start and end at any node in the tree.
;
;For example:
;Given the below binary tree,
;
;       1
;      / \
;     2   3
;
;Return 6.

(require "lib/binary-tree.rkt")

(define [max-path-sum root]
  (define [split n left-bnode right-bnode]
    (let ([left-path-sums (map (λ [np] (apply + (map bnode-payload np)))
                               (btree-paths left-bnode))]
          [right-path-sums (map (λ [np] (apply + (map bnode-payload np)))
                                (btree-paths right-bnode))])
      (max (+ n (apply max left-path-sums) (apply max right-path-sums))
           (max-path-sum left-bnode)
           (max-path-sum right-bnode))))

  (unless [null? root]
    (let ([n (bnode-payload root)])
      (match (list (bnode-left root) (bnode-right root))
        [(list '() '()) n]
        [(list '() r) (max-path-sum r)]
        [(list l '()) (max-path-sum l)]
        [(list l r) (split n l r)]))))

(define test-tree-a (btree-parse '(1 2 3)))
(define test-tree-b (btree-parse '(1 1 2 1 6 10 11)))

(max-path-sum test-tree-a)
(max-path-sum test-tree-b)
