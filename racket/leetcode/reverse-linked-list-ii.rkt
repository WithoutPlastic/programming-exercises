#lang racket

;Problem:
;Reverse a linked list from position m to n. Do it in-place and in one-pass.
;
;For example:
;Given 1->2->3->4->5->NULL, m = 2 and n = 4,
;
;return 1->4->3->2->5->NULL.
;
;Note:
;Given m, n satisfy the following condition:
;1 ≤ m ≤ n ≤ length of list.

(require "lib/linked-node.rkt")

(define [reverse-between! linked-list from to]
  (define [n-lnode-next n] (apply compose (make-list n lnode-next)))

  (define [rotate phead head ptail tail]
    (lnode-set-next! phead ptail)
    (lnode-set-next! head tail))

  (define [reverse-walk phead]
    (let* ([p-lnode (lnode-next phead)]
           [lnode (lnode-next p-lnode)]
           [n-lnode (lnode-next lnode)])
      (define [walk h t f repeat-cnt]
        (if [< 0 repeat-cnt]
          (begin (lnode-set-next! t h)
                 (walk t f (lnode-next f) (sub1 repeat-cnt)))
          (values phead p-lnode h t)))

      (walk p-lnode lnode n-lnode (- to from))))

  (define [seek] ((n-lnode-next (sub1 from)) linked-list))

  (call-with-values (λ _ (reverse-walk (seek))) rotate))

(define test-linked-list (new-linked-list 1 2 3 4 5))

(reverse-between! test-linked-list 2 4)
(displayln test-linked-list)
