#lang racket

;Utils
(define [repeat f n] (apply compose (make-list n f)))

;Exports
(define make-lnode mcons)
(define lnode-payload mcar)
(define lnode-next mcdr)
(define lnode-set-payload! set-mcar!)
(define lnode-set-next! set-mcdr!)
(define lnode-last? (compose null? lnode-next))
(define [new-linked-nodes lst]
  (foldr (λ [n pn] (lnode-set-next! n pn) n) '()
         (map (curryr make-lnode '()) lst)))
(define [linked-node-seek head v]
  (define [walk lnode]
    (cond ([null? lnode] '())
          ([equal? (lnode-payload lnode) v] lnode)
          (else (walk (lnode-next lnode)))))

  (walk head))
(define [linked-nodes-append! from-lnode to-lnode]
  (define [seek-last n] (if [lnode-last? n] n (seek-last (lnode-next n))))
  (lnode-set-next! (seek-last from-lnode) to-lnode))
(define [linked-nodes-length head]
  (define [iter lnode cnt]
    (if [null? lnode] cnt (iter (lnode-next lnode) (add1 cnt))))

  (iter head 0))
(define [linked-nodes-reverse! head]
  (unless [or [null? head] [lnode-last? head]]
    (let ([first-lnode head] [second-lnode (lnode-next head)])
      (linked-nodes-reverse! second-lnode)
      (lnode-set-next! second-lnode first-lnode)
      (lnode-set-next! first-lnode '()))))
(define [linked-nodes-last head]
  (if [lnode-last? head] head (linked-nodes-last (lnode-next head))))
(define [linked-nodes-ref head idx] ((repeat lnode-next idx) head))

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
(define [linked-list-append! from-linked-list to-linked-list]
  (linked-nodes-append! from-linked-list (linked-list-body to-linked-list)))
(define linked-list-length (compose sub1 linked-nodes-length))
(define linked-list-last linked-nodes-last)
(define [linked-list-ref linked-list idx]
  (linked-nodes-ref (linked-list-body linked-list) idx))
(define [linked-list-reverse! linked-list]
  (let ([last-lnode (linked-list-last linked-list)])
    (linked-nodes-reverse! (linked-list-body linked-list))
    (lnode-set-next! linked-list last-lnode)))

(provide (all-defined-out))
