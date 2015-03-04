#lang racket

;Problem:
;You are given two linked lists representing two non-negative numbers. The
;digits are stored in reverse order and each of their nodes contain a single
;digit. Add the two numbers and return it as a linked list.

(require "lib/linked-node.rkt")

(define [add-two-numbers lnodes-a lnodes-b]
  (define [iter remaining-a remaining-b cin]
    (let* ([a (lnode-payload remaining-a)]
           [b (lnode-payload remaining-b)]
           [total (+ a b cin)]
           [next-cin (floor (/ total 10))]
           [result (remainder total 10)])
      (cond ([lnode-last? remaining-a]
             (make-lnode result (lnode-next remaining-b)))
            ([lnode-last? remaining-b]
             (make-lnode result (lnode-next remaining-a)))
            (else (make-lnode result (iter (lnode-next remaining-a)
                                           (lnode-next remaining-b)
                                           next-cin))))))

  (iter lnodes-a lnodes-b 0))

(define nums-a (new-linked-nodes '(2 4 3)))
(define nums-b (new-linked-nodes '(5 6 4)))
(define nums-c (new-linked-nodes '(7 5 1 8)))

(add-two-numbers nums-c nums-b)
