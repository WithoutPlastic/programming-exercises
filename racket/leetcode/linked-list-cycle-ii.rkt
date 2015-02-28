#lang racket

;Problem:
;Given a linked list, return the node where the cycle begins. If there is no
;cycle, return null.
;
;Follow up:
;Can you solve it without using extra space?

(require "lib/linked-node.rkt")

(define [seek-linked-list-cycle linked-list]
  (define [repeat f n] (foldl compose identity (make-list n f)))
  (define double-next (compose lnode-next lnode-next))
  (define [find b f] (if [eq? b f] b (find (lnode-next b) (lnode-next f))))
  (define [get-cyclen-len b n c]
    (if [eq? n b] c (get-cyclen-len b (lnode-next n) (add1 c))))

  (define [walk-forward bn fn]
    (if [or [lnode-last? fn] [lnode-last? (lnode-next fn)]] '()
      (let ([next-bn (lnode-next bn)] [next-fn (double-next fn)])
        (if [eq? next-bn next-fn]
          (find linked-list
                ((repeat lnode-next
                         (get-cyclen-len next-bn (lnode-next next-bn) 1))
                 linked-list))
          (walk-forward next-bn next-fn)))))

  (walk-forward linked-list linked-list))

(define test-linked-list (apply new-linked-list (range 0 10)))
(define test-cycle-linked-list
  (let ([ll (apply new-linked-list (range 0 10))])
    (lnode-set-next! (linked-list-seek ll 9) (linked-list-seek ll 5))
    ll))

(seek-linked-list-cycle test-linked-list)
(seek-linked-list-cycle test-cycle-linked-list)
