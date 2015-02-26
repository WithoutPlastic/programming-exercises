#lang racket

;Problem:
;Given n, generate all structurally unique BST's (binary search trees) that
;store values 1...n.
;
;For example,
;Given n = 3, your program should return all 5 unique BST's shown below.
;
;   1         3     3      2      1
;    \       /     /      / \      \
;     3     2     1      1   3      2
;    /     /       \                 \
;   2     1         2                 3
;
;confused what "{1,#,2,3}" means?
;> read more on how binary tree is serialized on OJ.
;
;OJ's Binary Tree Serialization:
;
;The serialization of a binary tree follows a level order traversal, where '#'
;signifies a path terminator where no node exists below.
;
;Here's an example:
;
;   1
;  / \
; 2   3
;    /
;   4
;    \
;     5
;
;The above binary tree is serialized as "{1,2,3,#,#,4,#,#,5}".

(require "lib/binary-tree.rkt")

(define [generate-binary-search-trees n]
  (define [btree-permute nums]
    (define [select num]
      (let ([left-slice (takef nums (curryr < num))]
            [right-slice (dropf nums (curryr <= num))])
        (append-map (λ [lb] (map (λ [rb] (make-btree-node num lb rb))
                                 (btree-permute right-slice)))
                    (btree-permute left-slice))))

    (if [null? nums] (list '()) (append-map select nums)))

  (btree-permute (range 1 (add1 n))))

(for-each (compose displayln btree-serialize) (generate-binary-search-trees 3))
(displayln "----------")
(for-each (compose displayln btree-serialize) (generate-binary-search-trees 4))
