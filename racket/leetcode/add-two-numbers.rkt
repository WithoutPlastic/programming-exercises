#lang racket

;Problem:
;You are given two linked lists representing two non-negative numbers. The
;digits are stored in reverse order and each of their nodes contain a single
;digit. Add the two numbers and return it as a linked list.

(require "lib/linked-node.rkt")

(define [add-two-numbers num-a num-b]
  (define [iter remaining-a remaining-b cin]
    (let* ([a (node-payload remaining-a)]
           [b (node-payload remaining-b)]
           [total (+ a b cin)]
           [next-cin (floor (/ total 10))]
           [result (remainder total 10)])
      (cond ([last-node? remaining-a]
             (make-node result (node-next remaining-b)))
            ([last-node? remaining-b]
             (make-node result (node-next remaining-a)))
            (else (make-node result
                             (iter (node-next remaining-a)
                                   (node-next remaining-b)
                                   next-cin))))))
  (iter num-a num-b 0))

(define num-a (make-node 2 (make-node 4 (make-node 3 '()))))
(define num-b (make-node 5 (make-node 6 (make-node 4 '()))))
(define num-c (make-node 7 (make-node 5 (make-node 1 (make-node 8 '())))))

(add-two-numbers num-c num-b)
