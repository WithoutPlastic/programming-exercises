#lang racket

;Problem:
;Given an array where elements are sorted in ascending order, convert it to a
;height balanced BST.

(require "lib/binary-tree.rkt")

(define [sorted-list->bst lst]
  (let ([len (length lst)])
    (define [split]
      (let* ([mid-idx (floor (/ len 2))] [mid-elt (list-ref lst mid-idx)]
             [left-slice (take lst mid-idx)]
             [right-slice (drop lst (add1 mid-idx))]
             [new-node (make-btree-alone-node mid-elt)])
        (btree-set-left! new-node (sorted-list->bst left-slice))
        (btree-set-right! new-node (sorted-list->bst right-slice))
        new-node))

    (cond ([= len 1] (make-btree-alone-node (car lst)))
          ([< 2 len] (split))
          (else (let ([p-node (make-btree-alone-node (cadr lst))]
                      [l-node (make-btree-alone-node (car lst))])
                  (btree-set-left! p-node l-node) p-node)))))

(define test-list (range 0 24))

(sorted-list->bst test-list)
