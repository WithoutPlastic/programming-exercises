#lang racket

;Problem:
;Merge two sorted linked lists and return it as a new list. The new list should
;be made by splicing together the nodes of the first two list.

(require "linked-node.rkt")

(define [merge-two-sorted-lists linked-list-a linked-list-b]
  (let ([head-a (linked-list-body linked-list-a)]
        [head-b (linked-list-body linked-list-b)])
    (define [iter remaining-a remaining-b]
      (define [continue]
        (let ([f-a (node-payload remaining-a)] [rest-a (node-next remaining-a)]
              [f-b (node-payload remaining-b)] [rest-b (node-next remaining-b)])
          (if [< f-a f-b]
            (begin (set-node-next! remaining-a (iter rest-a remaining-b))
                   remaining-a)
            (begin (set-node-next! remaining-b (iter remaining-a rest-b))
                   remaining-b))))

      (cond ([last-node? remaining-a] remaining-b)
            ([last-node? remaining-b] remaining-a)
            (else (continue))))

    (make-linked-list (iter head-a head-b))))

(define test-a
  (make-linked-list
    (make-node -10 (make-node -8 (make-node -4 (make-node -1 (make-node 0
    (make-node 0 (make-node 1 (make-node 7 (make-node 9 '())))))))))))
(define test-b
  (make-linked-list
    (make-node -9 (make-node -7 (make-node -4 (make-node -1 (make-node 1
    (make-node 2 (make-node 4 (make-node 6 (make-node 6 '())))))))))))

(merge-two-sorted-lists test-a test-b)
