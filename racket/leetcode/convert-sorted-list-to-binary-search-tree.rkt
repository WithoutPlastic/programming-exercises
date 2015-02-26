#lang racket

;Problem:
;Given a singly linked list where elements are sorted in ascending order,
;convert it to a height balanced BST.

(require "lib/linked-node.rkt")
(require "lib/binary-tree.rkt")

(define [sorted-linked-list->bst linked-nodes]
  (define double-lnode-next (compose lnode-next lnode-next))
  (define [walk backward-lnode forward-lnode]
    (define [one-lnode-left? node]
      [and [not [lnode-last? node]] [lnode-last? (lnode-next node)]])

    (if [or [lnode-last? forward-lnode] [one-lnode-left? forward-lnode]]
      (split backward-lnode)
      (walk (lnode-next backward-lnode) (double-lnode-next forward-lnode))))

  (define [split pre-mid-lnode]
    (let* ([mid-lnode (lnode-next pre-mid-lnode)]
           [post-mid-lnode (lnode-next mid-lnode)]
           [mid-payload (lnode-payload mid-lnode)]
           [new-bnode (make-btree-alone-node mid-payload)])
      (lnode-set-next! pre-mid-lnode '())
      (bnode-set-left! new-bnode (sorted-linked-list->bst linked-nodes))
      (bnode-set-right! new-bnode (sorted-linked-list->bst post-mid-lnode))
      new-bnode))

  (let* ([first-lnode linked-nodes]
         [second-lnode (lnode-next first-lnode)]
         [first-payload (lnode-payload first-lnode)])
    (cond ([lnode-last? first-lnode] (make-btree-alone-node first-payload))
          ([not [lnode-last? second-lnode]] (walk first-lnode second-lnode))
          (else
            (let* ([second-payload (lnode-payload second-lnode)]
                   [p-bnode (make-btree-alone-node second-payload)])
              (bnode-set-left! p-bnode (make-btree-alone-node first-payload))
              p-bnode)))))

(define test-linked-nodes (apply new-linked-nodes (range 0 24)))

(btree-serialize (sorted-linked-list->bst test-linked-nodes))
