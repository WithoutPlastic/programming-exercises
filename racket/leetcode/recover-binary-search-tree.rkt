#lang racket

;Problem:
;Two elements of a binary search tree (BST) are swapped by mistake.
;
;Recover the tree without changing its structure.
;Note:
;A solution using O(n) space is pretty straight forward. Could you devise a
;constant space solution?
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

(define [recover-bst root]
  (define [scan node verdict?]
    (if [null? node]
      false
      [or [and [verdict? (bnode-payload node)] node]
          (scan (bnode-left node) verdict?)
          (scan (bnode-right node) verdict?)]))

  (define [swap node-a node-b]
    (let ([old-v-a (bnode-payload node-a)]
          [old-v-b (bnode-payload node-b)])
      (bnode-set-payload! node-a old-v-b)
      (bnode-set-payload! node-b old-v-a)
      (cons old-v-a old-v-b)))

  (define [continue]
    (let* ([payload (bnode-payload root)]
           [left-br (bnode-left root)] [right-br (bnode-right root)]
           [scan-left-result (scan left-br (curry < payload))]
           [scan-right-result (scan right-br (curryr < payload))])
      (cond ([and scan-left-result [not scan-right-result]]
             (swap root scan-left-result))
            ([and [not scan-left-result] scan-right-result]
             (swap root scan-right-result))
            ([and scan-left-result scan-right-result]
             (swap scan-left-result scan-right-result))
            (else [or (recover-bst left-br) (recover-bst right-br)]))))

  (if [null? root] false (continue)))

(define test-tree-a
  (btree-parse
    '(7 5 17 3 10 12 20 1 4 6 9 11 13 18 24 - - - - - - - - - - - 15)))
(define test-tree-b
  (btree-parse
    '(10 5 17 3 7 12 20 1 24 6 9 11 13 18 4 - - - - - - - - - - - 15)))
(define test-tree-c
  (btree-parse
    '(11 5 17 3 7 12 20 1 4 6 9 10 13 18 24 - - - - - - - - - - - 15)))
(define test-tree-d
  (btree-parse
    '(10 5 17 3 7 12 20 1 4 9 6 11 13 18 24 - - - - - - - - - - - 15)))

(recover-bst test-tree-a)
(recover-bst test-tree-b)
(recover-bst test-tree-c)
(recover-bst test-tree-d)
