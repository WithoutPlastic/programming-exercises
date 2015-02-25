#lang racket

;Utils
(define [make-btree-node payload left-node right-node]
  (mcons payload (mcons left-node right-node)))
(define make-btree-alone-node (curryr make-btree-node '() '()))
(define [make-btree-empty-node] (make-btree-alone-node '()))

(define btree-payload mcar)
(define btree-branches mcdr)
(define btree-left (compose mcar btree-branches))
(define btree-right (compose mcdr btree-branches))

(define [btree->branches x] (list (btree-left x) (btree-right x)))

(define btree-payload-empty? (compose null? btree-payload))
(define btree-left-empty? (compose null? btree-left))
(define btree-right-empty? (compose null? btree-right))
(define [btree-branches-non-empty? x]
  [nor [btree-left-empty? x] [btree-right-empty? x]])
(define [btree-last? x]
  [and [btree-left-empty? x] [btree-right-empty? x]])

(define [btree-set-left! node v] (set-mcar! (btree-branches node) v))
(define [btree-set-right! node v] (set-mcdr! (btree-branches node) v))
(define [btree-set-branches! node lv rv]
  (btree-set-left! node lv) (btree-set-right! node rv))

(define [btree-height root]
  (let ([left (btree-left root)] [right (btree-right root)])
    (add1 (cond ([btree-last? root] 0)
                ([btree-branches-non-empty? root]
                 (max (btree-height left) (btree-height right)))
                ([btree-left-empty? root] (btree-height right))
                (else (btree-height left))))))

(define padding-sym '-)
(define padding-sym? (curry eq? padding-sym))

(define [btree-serialize root]
  (define [null->padding-node x]
    (if [null? x] (make-btree-alone-node padding-sym) x))

  (define [iter-level pending-nodes]
    (let* ([next-pendings (append-map btree->branches pending-nodes)]
           [filled-pendings (map null->padding-node next-pendings)]
           [payloads (map btree-payload filled-pendings)])
      (if [findf (negate null?) next-pendings]
        (append payloads (iter-level filled-pendings))
        '())))

  (cons (btree-payload root) (iter-level (list root))))

(define [btree-parse lst]
  (define [padding-elt? idx] [eq? (list-ref lst idx) padding-sym])
  (define [next-idx-pair idx] (list (sub1 (* (add1 idx) 2)) (* (add1 idx) 2)))

  (define [make-pair-nodes p]
    (let ([l-elt (list-ref lst (car p))] [r-elt (list-ref lst (cadr p))])
      (list (if [padding-sym? l-elt] '() (make-btree-alone-node l-elt))
            (if [padding-sym? r-elt] '() (make-btree-alone-node r-elt)))))

  (define [iter nodes idxs]
    (when [< (cadr (next-idx-pair (last idxs))) (length lst)]
      (let* ([next-idx-pairs (map next-idx-pair idxs)]
             [next-node-pairs (map make-pair-nodes next-idx-pairs)]
             [all-idxs (apply append next-idx-pairs)]
             [all-nodes (apply append next-node-pairs)]
             [valid-next-idxs (filter-not padding-elt? all-idxs)]
             [valid-next-nodes (filter-not null? all-nodes)])
        (for-each (λ [n p] (btree-set-branches! n (car p) (cadr p)))
                  nodes next-node-pairs)
        (iter valid-next-nodes valid-next-idxs))))

  (when [not [null? lst]]
    (let ([root (make-btree-alone-node (car lst))])
      (iter (list root) (list 0))
      root)))

(provide (all-defined-out))
