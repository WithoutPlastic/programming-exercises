#lang racket

;Problem:
;Sort a linked list in O(n log n) time using constant space complexity.

(require "lib/linked-node.rkt")

(define [binary-sort-linked-list! linked-list compare?]
  (define [merge! lnodes-a lnodes-b result]
    (cond ([null? lnodes-a] (lnode-set-next! result lnodes-b))
          ([null? lnodes-b] (lnode-set-next! result lnodes-a))
          ([compare? (lnode-payload lnodes-a) (lnode-payload lnodes-b)]
           (lnode-set-next! result lnodes-a)
           (merge! (lnode-next lnodes-a) lnodes-b lnodes-a))
          (else (lnode-set-next! result lnodes-b)
                (merge! (lnode-next lnodes-b) lnodes-a lnodes-b))))

  (define [split-and-merge! len]
    (let* ([mid-lnode (linked-list-ref linked-list (floor (/ len 2)))]
           [tail-part (make-linked-list (lnode-next mid-lnode))])
      (lnode-set-next! mid-lnode '())
      (binary-sort-linked-list! linked-list compare?)
      (binary-sort-linked-list! tail-part compare?)
      (merge! (linked-list-body linked-list)
              (linked-list-body tail-part)
              linked-list)))

  (let ([linked-list-len (linked-list-length linked-list)])
    (unless [< linked-list-len 2]
      (if [< 2 linked-list-len] (split-and-merge! linked-list-len)
        (let* ([first-lnode (linked-list-body linked-list)]
               [second-lnode (lnode-next first-lnode)])
          (unless [compare? (lnode-payload first-lnode)
                            (lnode-payload second-lnode)]
            (linked-list-set-body! linked-list second-lnode)
            (lnode-set-next! second-lnode first-lnode)
            (lnode-set-next! first-lnode '())))))))

(define test-linked-list (new-linked-list (build-list 20 (thunk* (random 10)))))

(displayln test-linked-list)
(binary-sort-linked-list! test-linked-list <)
(displayln test-linked-list)
