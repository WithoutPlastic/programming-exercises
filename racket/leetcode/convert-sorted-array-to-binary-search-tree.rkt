#lang racket

;Problem:
;Given an array where elements are sorted in ascending order, convert it to a
;height balanced BST.

(require "lib/binary-tree.rkt")

(define [sorted-list->bst lst]
  (define [split mid-idx]
    (let* ([mid-elt (list-ref lst mid-idx)]
           [left-slice (take lst mid-idx)]
           [right-slice (drop lst (add1 mid-idx))]
           [new-bnode (make-btree-alone-node mid-elt)])
      (bnode-set-left! new-bnode (sorted-list->bst left-slice))
      (bnode-set-right! new-bnode (sorted-list->bst right-slice))
      new-bnode))

  (let ([len (length lst)])
    (cond ([= len 1] (make-btree-alone-node (car lst)))
          ([< 2 len] (split (floor (/ len 2))))
          (else (let ([p-bnode (make-btree-alone-node (cadr lst))]
                      [l-bnode (make-btree-alone-node (car lst))])
                  (bnode-set-left! p-bnode l-bnode) p-bnode)))))

(define test-list (range 0 24))

(btree-serialize (sorted-list->bst test-list))
