#lang racket

;Problem:
;A linked list is given such that each node contains an additional random
;pointer which could point to any node in the list or null.
;
;Return a deep copy of the list.

(define [make-lnode payload next rand-next]
  (mcons payload (mcons next rand-next)))
(define lnode-payload mcar)
(define lnode-next (compose mcar mcdr))
(define lnode-rand-next (compose mcdr mcdr))
(define lnode-set-payload! set-mcar!)
(define [lnode-set-next! lnode next] (set-mcar! (mcdr lnode) next))
(define [lnode-set-rand-next! lnode rand-next]
  (set-mcdr! (mcdr lnode) rand-next))

(define [copy-random-list head]
  (define [restore! lnode]
    (unless [null? lnode]
      (let ([copied-lnode (lnode-next lnode)]
            [nn-lnode (lnode-next (lnode-next lnode))])
        (lnode-set-next! lnode nn-lnode)
        (unless [null? nn-lnode]
          (lnode-set-next! copied-lnode (lnode-next nn-lnode)))
        (restore! nn-lnode))))

  (define [set-rand-nexts! lnode]
    (unless [null? lnode]
      (lnode-set-rand-next! (lnode-next lnode)
                            (lnode-next (lnode-rand-next lnode)))
      (set-rand-nexts! (lnode-next (lnode-next lnode)))))

  (define [copy-backbone lnode]
    (if [null? lnode] '()
      (let ([new-lnode (make-lnode (lnode-payload lnode) '() '())])
        (lnode-set-next! new-lnode (lnode-next lnode))
        (lnode-set-next! lnode new-lnode)
        (copy-backbone (lnode-next new-lnode))
        new-lnode)))

  (let ([new-list (copy-backbone head)])
    (set-rand-nexts! head)
    (restore! head)
    new-list))

(define [gen-test-linked-nodes]
  (let* ([fifth-lnode (make-lnode 5 '() '())]
         [fourth-lnode (make-lnode 4 fifth-lnode '())]
         [third-lnode (make-lnode 3 fourth-lnode '())]
         [second-lnode (make-lnode 2 third-lnode '())]
         [first-lnode (make-lnode 1 second-lnode '())])
    (lnode-set-rand-next! first-lnode third-lnode)
    (lnode-set-rand-next! second-lnode first-lnode)
    (lnode-set-rand-next! third-lnode fifth-lnode)
    (lnode-set-rand-next! fourth-lnode third-lnode)
    (lnode-set-rand-next! fifth-lnode second-lnode)
    first-lnode))
(define test-linked-nodes (gen-test-linked-nodes))
(displayln test-linked-nodes)

(let ([new-list (copy-random-list test-linked-nodes)])
  (displayln new-list)
  [eq? new-list test-linked-nodes])
