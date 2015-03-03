#lang racket

;Problem:
;Sort a linked list using insertion sort.

(require "lib/linked-node.rkt")

(define [insertion-sort-linked-list linked-nodes compare?]
  (define [insert-lnode! result lnode]
    (let ([next-lnode (lnode-next result)])
      (cond ([lnode-last? result]
             (lnode-set-next! result lnode)
             (lnode-set-next! lnode '()))
            ([compare? (lnode-payload next-lnode) (lnode-payload lnode)]
             (insert-lnode! next-lnode lnode))
            (else (lnode-set-next! result lnode)
                  (lnode-set-next! lnode next-lnode)))))

  (define [iter unsort-lnodes result]
    (if [null? unsort-lnodes] result
      (let ([rests (lnode-next unsort-lnodes)])
        (insert-lnode! result unsort-lnodes)
        (iter rests result))))

  (linked-list-body (iter linked-nodes (make-linked-list '()))))

(insertion-sort-linked-list
  (apply new-linked-nodes (build-list 20 (thunk* (random 10)))) <)
