#lang racket

;Problem:
;Given a singly linked list where elements are sorted in ascending order,
;convert it to a height balanced BST.

(require "lib/linked-node.rkt")
(require "lib/binary-tree.rkt")

(define double-node-next (compose node-next node-next))

(define [one-node-left? node]
  [and [not [last-node? node]] [last-node? (node-next node)]])

(define [sorted-linked-list->bst linked-list]
  (let* ([first-node (node-next linked-list)]
         [second-node (node-next first-node)]
         [first-payload (node-payload first-node)])
    (define [split before-mid-node]
      (let* ([mid-node (node-next before-mid-node)]
             [after-mid-node (node-next mid-node)]
             [mid-payload (node-payload mid-node)]
             [new-node (make-btree-alone-node mid-payload)]
             [right-linked-list (make-linked-list after-mid-node)])
        (set-node-next! before-mid-node '())
        (btree-set-left! new-node (sorted-linked-list->bst linked-list))
        (btree-set-right! new-node (sorted-linked-list->bst right-linked-list))
        new-node))

    (define [walk backward-node forward-node]
      (if [or [last-node? forward-node] [one-node-left? forward-node]]
        (split backward-node)
        (walk (node-next backward-node) (double-node-next forward-node))))

    (cond ([last-node? first-node] (make-btree-alone-node first-payload))
          ([not [last-node? second-node]] (walk linked-list linked-list))
          (else (let* ([second-payload (node-payload second-node)]
                       [p-node (make-btree-alone-node second-payload)])
                  (btree-set-left! p-node (make-btree-alone-node first-payload))
                  p-node)))))

(define test-linked-list
  (new-linked-list
    0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23))

(sorted-linked-list->bst test-linked-list)
