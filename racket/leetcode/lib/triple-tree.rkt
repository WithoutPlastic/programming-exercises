#lang racket

;Utils
(define [make-tnode payload next left right]
  (mcons payload (mcons next (mcons left right))))
(define tnode-payload mcar)
(define tnode-next (compose mcar mcdr))
(define tnode-left (compose mcar mcdr mcdr))
(define tnode-right (compose mcdr mcdr mcdr))
(define tnode-set-payload! set-mcar!)
(define [tnode-set-next! tnode x] (set-mcar! (mcdr tnode) x))
(define [tnode-set-left! tnode x] (set-mcar! (mcdr (mcdr tnode)) x))
(define [tnode-set-right! tnode x] (set-mcdr! (mcdr (mcdr tnode)) x))

(provide (all-defined-out))
