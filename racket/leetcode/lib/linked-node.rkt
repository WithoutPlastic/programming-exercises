#lang racket

;Utils
(define make-lnode mcons)
(define lnode-payload mcar)
(define lnode-next mcdr)
(define lnode-set-payload! set-mcar!)
(define lnode-set-next! set-mcdr!)
(define lnode-last? (compose null? lnode-next))
(define [new-linked-nodes . args]
  (foldr (λ [n pn] (lnode-set-next! n pn) n) '()
         (map (curryr make-lnode '()) args)))
(define [linked-node-seek head v]
  (define [walk lnode]
    (cond ([null? lnode] '())
          ([equal? (lnode-payload lnode) v] lnode)
          (else (walk (lnode-next lnode)))))

  (walk head))

(define make-linked-list (curry mcons 'linked-list))
(define linked-list-head lnode-payload)
(define linked-list-body lnode-next)
(define linked-list-set-body! lnode-set-next!)
(define linked-list-empty? lnode-last?)
(define new-linked-list (compose make-linked-list new-linked-nodes))
(define [linked-list-seek linked-list v #:position [pos 'default]]
  (define [previous-walk lnode]
    (let ([next-lnode (lnode-next lnode)])
      (cond ([null? next-lnode] '())
            ([equal? (lnode-payload next-lnode) v] lnode)
            (else (previous-walk next-lnode)))))

  (case pos
    [(previous) (previous-walk linked-list)]
    [(default) (linked-node-seek (linked-list-body linked-list) v)]
    [else (error "position keyword incorrect -- LINKED-LIST-SEEK" pos)]))

(provide (all-defined-out))
