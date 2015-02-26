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

(define make-linked-list (curry mcons 'linked-list))
(define linked-list-head lnode-payload)
(define linked-list-body lnode-next)
(define linked-list-set-body! lnode-set-next!)
(define linked-list-empty? lnode-last?)
(define new-linked-list (compose make-linked-list new-linked-nodes))

(provide (all-defined-out))
