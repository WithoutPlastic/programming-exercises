#lang racket

;Utils
(define [btree-node? obj] [and [mpair? obj] [mpair? (mcdr mpair?)]])
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

(define btree-set-payload! set-mcar!)
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
  (define [->result x] (if [null? x] padding-sym (btree-payload x)))

  (define [iter-level pending-nodes]
    (let* ([exted-pendings (append-map btree->branches pending-nodes)]
           [results (map ->result exted-pendings)]
           [next-pendings (filter-not null? exted-pendings)])
      (if [null? next-pendings]
        '()
        (append results (iter-level next-pendings)))))

  (cons (btree-payload root)
        (dropf-right (iter-level (list root)) padding-sym?)))

(define [btree-parse lst]
  (define padded-lst (append lst (list padding-sym)))
  (define lst-len (length padded-lst))
  (define [padding-elt? idx]
    [eq? (list-ref padded-lst idx) padding-sym])

  (define [gen-idx-pairs base cnt]
    (map (λ [s] (map (curry + 1 base) (list (* s 2) (add1 (* s 2)))))
         (range 0 cnt)))

  (define [make-pair-nodes p]
    (let ([l-elt (list-ref padded-lst (car p))]
          [r-elt (list-ref padded-lst (cadr p))])
      (list (if [padding-sym? l-elt] '() (make-btree-alone-node l-elt))
            (if [padding-sym? r-elt] '() (make-btree-alone-node r-elt)))))

  (define [iter nodes base-idx]
    (let* ([node-cnt (length nodes)]
           [exted-idx-pairs (gen-idx-pairs base-idx node-cnt)]
           [next-base-idx (cadr (last exted-idx-pairs))])
      (when [< next-base-idx lst-len]
        (let* ([exted-node-pairs (map make-pair-nodes exted-idx-pairs)]
               [flattened-nodes (flatten exted-node-pairs)]
               [next-nodes (filter-not null? flattened-nodes)])
          (for-each (λ [n p] (btree-set-branches! n (car p) (cadr p)))
                    nodes exted-node-pairs)
          (iter next-nodes next-base-idx)))))

  (let ([root (make-btree-alone-node (car padded-lst))])
    (iter (list root) 0)
    root))

(define [btree-equal? root-a root-b]
  (define [continue]
    (let ([a-left-br (btree-left root-a)] [a-right-br (btree-right root-a)]
          [b-left-br (btree-left root-b)] [b-right-br (btree-right root-b)]
          [payload-a (btree-payload root-a)] [payload-b (btree-payload root-b)])
      [and [equal? payload-a payload-b]
           [btree-equal? a-left-br b-left-br]
           [btree-equal? a-right-br b-right-br]]))

  (cond ([and [null? root-a] [null? root-b]] true)
        ([or [null? root-a] [null? root-b]] false)
        (else (continue))))

(provide (all-defined-out))
