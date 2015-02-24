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

(require "linked-node.rkt")

(define [repeat p n] (if [< 1 n] (compose p (repeat p (sub1 n))) p))

(define [reverse-between! linked-list from to]
  (define [rotate head-stub head tail-stub tail]
    (set-node-next! head-stub tail-stub)
    (set-node-next! head tail))

  (define [reverse-walk head-stub]
    (let* ([r-head (node-next head-stub)]
           [r-tail (node-next r-head)]
           [scout-node (node-next r-tail)])
      (define [walk h t s repeat-cnt]
        (if [< 0 repeat-cnt]
          (begin (set-node-next! t h)
                 (walk t s (node-next s) (sub1 repeat-cnt)))
          (values head-stub r-head h t)))

      (walk r-head r-tail scout-node (- to from))))

  (define [seek-head] ((repeat node-next (sub1 from)) linked-list))

  (call-with-values (λ _ (reverse-walk (seek-head))) rotate))

(define test-linked-list (new-linked-list 1 2 3 4 5))

(displayln test-linked-list)
(reverse-between! test-linked-list 2 4)
(displayln test-linked-list)
