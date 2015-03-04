#lang racket

;Problem:
;Given n, how many structurally unique BST's (binary search trees) that store
;values 1...n?
;
;For example,
;Given n = 3, there are a total of 5 unique BST's.
;
;   1         3     3      2      1
;    \       /     /      / \      \
;     3     2     1      1   3      2
;    /     /       \                 \
;   2     1         2                 3
;

(require "lib/binary-tree.rkt")

(define [num-trees n]
  (define [btree-permute nums]
    (define [select num]
      (let ([left-slice (takef nums (curryr < num))]
            [right-slice (dropf nums (curryr <= num))])
        (append-map (λ [lb] (map (λ [rb] (make-btree-node num lb rb))
                                 (btree-permute right-slice)))
                    (btree-permute left-slice))))

    (if [null? nums] (list '()) (append-map select nums)))

  (length (btree-permute (range 1 (add1 n)))))

(num-trees 3)
(num-trees 4)
