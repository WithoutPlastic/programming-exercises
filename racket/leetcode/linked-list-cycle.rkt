#lang racket

;Problem:
;Given a linked list, determine if it has a cycle in it.
;
;Follow up:
;Can you solve it without using extra space?

(require "lib/linked-node.rkt")

(define [linked-list-cycle? linked-list]
  (define double-next (compose lnode-next lnode-next))
  (define [walk bn fn]
    (if [or [lnode-last? fn] [lnode-last? (lnode-next fn)]] false
      (let ([next-bn (lnode-next bn)] [next-fn (double-next fn)])
        (if [eq? (lnode-payload next-bn) (lnode-payload next-fn)]
          true (walk next-bn next-fn)))))

  (walk linked-list linked-list))

(define test-linked-list (apply new-linked-list (range 0 10)))
(define test-cycle-linked-list
  (let ([ll (apply new-linked-list (range 0 10))])
    (lnode-set-next! (linked-list-seek ll 9) (linked-list-seek ll 5))
    ll))

(linked-list-cycle? test-linked-list)
(linked-list-cycle? test-cycle-linked-list)
