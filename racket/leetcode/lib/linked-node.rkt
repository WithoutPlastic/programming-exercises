#lang racket

;Utils
(define node-payload mcar)
(define node-next mcdr)
(define [set-node-payload! node payload] (set-mcar! node payload))
(define [set-node-next! node next] (set-mcdr! node next))
(define [make-node payload next] (mcons payload next))
(define [last-node? node] [null? (node-next node)])
(define [make-linked-list node] (mcons 'linked-list node))
(define linked-list-body mcdr)
(define [new-linked-list . args]
  (define [iter remaining]
    (if [null? remaining]
      '()
      (make-node (car remaining) (iter (cdr remaining)))))

  (make-linked-list (iter args)))

(provide (all-defined-out))
