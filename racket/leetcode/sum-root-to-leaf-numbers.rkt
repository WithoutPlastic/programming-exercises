#lang racket

;Problem:
;Given a binary tree containing digits from 0-9 only, each root-to-leaf path
;could represent a number.
;
;An example is the root-to-leaf path 1->2->3 which represents the number 123.
;
;Find the total sum of all root-to-leaf numbers.
;
;For example,
;
;    1
;   / \
;  2   3
;
;The root-to-leaf path 1->2 represents the number 12.
;The root-to-leaf path 1->3 represents the number 13.
;
;Return the sum = 12 + 13 = 25.

(require "lib/binary-tree.rkt")

(define [sum-numbers root]
  (define [list->int lst]
    (let ([len (length lst)])
      (apply + (map * lst (reverse (map (curry expt 10) (range 0 len)))))))

  (apply + (map (λ [p] (list->int (map bnode-payload p))) (btree-paths root))))

(define test-tree (btree-parse '(1 2 3)))

(sum-numbers test-tree)
