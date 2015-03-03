#lang racket

;Problem:
;Given a singly linked list L: L0->L1->…->Ln-1->Ln,
;reorder it to: L0->Ln->L1->Ln-1->L2->Ln-2->…
;
;You must do this in-place without altering the nodes' values.
;
;For example,
;Given {1,2,3,4}, reorder it to {1,4,2,3}.

(require "lib/linked-node.rkt")

(define [reorder-linked-list head]
  (let* ([len (linked-nodes-length head)]
         [pre-mid-lnode (linked-nodes-ref head (floor (/ (sub1 len) 2)))]
         [mid-lnode (lnode-next pre-mid-lnode)]
         [last-lnode (linked-nodes-last head)])
    (define [iter head tail]
      (unless [or [null? head] [null? tail]]
        (let ([head-next-lnode (lnode-next head)]
              [tail-next-lnode (lnode-next tail)])
          (lnode-set-next! head tail)
          (lnode-set-next! tail head-next-lnode)
          (iter head-next-lnode tail-next-lnode))))

    (lnode-set-next! pre-mid-lnode '())
    (linked-nodes-reverse! mid-lnode)
    (iter head last-lnode)))

(reorder-linked-list (new-linked-nodes (range 0 9)))
