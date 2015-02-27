#lang racket

;Utils
(define [btree-node? obj] [and [mpair? obj] [mpair? (mcdr mpair?)]])
(define [make-btree-node payload left-node right-node]
  (mcons payload (mcons left-node right-node)))
(define make-btree-alone-node (curryr make-btree-node '() '()))
(define [make-btree-empty-node] (make-btree-alone-node '()))

(define bnode-payload mcar)
(define bnode-branches mcdr)
(define bnode-left (compose mcar bnode-branches))
(define bnode-right (compose mcdr bnode-branches))
(define [bnode->branches bnode] (list (bnode-left bnode) (bnode-right bnode)))

(define bnode-payload-empty? (compose null? bnode-payload))
(define bnode-left-empty? (compose null? bnode-left))
(define bnode-right-empty? (compose null? bnode-right))
(define [bnode-branches-non-empty? bnode]
  [nor [bnode-left-empty? bnode] [bnode-right-empty? bnode]])
(define [bnode-branches-empty? bnode]
  [and [bnode-left-empty? bnode] [bnode-right-empty? bnode]])
(define bnode-last? bnode-branches-empty?)

(define bnode-set-payload! set-mcar!)
(define [bnode-set-left! bnode x] (set-mcar! (bnode-branches bnode) x))
(define [bnode-set-right! bnode x] (set-mcdr! (bnode-branches bnode) x))
(define [bnode-set-branches! bnode lv rv]
  (bnode-set-left! bnode lv) (bnode-set-right! bnode rv))

(define [btree-height root]
  (let ([left (bnode-left root)] [right (bnode-right root)])
    (add1 (cond ([bnode-branches-empty? root] 0)
                ([bnode-branches-non-empty? root]
                 (max (btree-height left) (btree-height right)))
                ([bnode-left-empty? root] (btree-height right))
                (else (btree-height left))))))

(define [list->btree lst]
  (define [split mid-idx]
    (let* ([mid-elt (list-ref lst mid-idx)]
           [left-slice (take lst mid-idx)]
           [right-slice (drop lst (add1 mid-idx))]
           [new-bnode (make-btree-alone-node mid-elt)])
      (bnode-set-left! new-bnode (list->btree left-slice))
      (bnode-set-right! new-bnode (list->btree right-slice))
      new-bnode))

  (let ([len (length lst)])
    (cond ([= len 1] (make-btree-alone-node (car lst)))
          ([< 2 len] (split (floor (/ len 2))))
          (else (let ([p-bnode (make-btree-alone-node (cadr lst))]
                      [l-bnode (make-btree-alone-node (car lst))])
                  (bnode-set-left! p-bnode l-bnode) p-bnode)))))

(define padding-sym '-)
(define padding-sym? (curry eq? padding-sym))

(define [btree-serialize root]
  (define [null->padding-node bnode]
    (if [null? bnode] (make-btree-alone-node padding-sym) bnode))
  (define [->result bnode] (if [null? bnode] padding-sym (bnode-payload bnode)))

  (define [iter-level pending-nodes]
    (let* ([exted-pendings (append-map bnode->branches pending-nodes)]
           [results (map ->result exted-pendings)]
           [next-pendings (filter-not null? exted-pendings)])
      (if [null? next-pendings] '()
        (append results (iter-level next-pendings)))))

  (cons (bnode-payload root)
        (dropf-right (iter-level (list root)) padding-sym?)))

(define [btree-parse lst]
  (define padded-lst (append lst (list padding-sym)))
  (define lst-len (length padded-lst))
  (define [padding-elt? idx] [eq? (list-ref padded-lst idx) padding-sym])

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
          (for-each (λ [n p] (bnode-set-branches! n (car p) (cadr p)))
                    nodes exted-node-pairs)
          (iter next-nodes next-base-idx)))))

  (let ([root (make-btree-alone-node (car padded-lst))])
    (iter (list root) 0) root))

(define [btree-equal? root-a root-b]
  (define [continue]
    (let ([a-left-br (bnode-left root-a)] [a-right-br (bnode-right root-a)]
          [b-left-br (bnode-left root-b)] [b-right-br (bnode-right root-b)]
          [payload-a (bnode-payload root-a)] [payload-b (bnode-payload root-b)])
      [and [equal? payload-a payload-b]
           [btree-equal? a-left-br b-left-br]
           [btree-equal? a-right-br b-right-br]]))

  (cond ([and [null? root-a] [null? root-b]] true)
        ([or [null? root-a] [null? root-b]] false)
        (else (continue))))

(define [btree-swap-branches! root]
  (when [btree-node? root]
    (let ([old-left-br (bnode-left root)]
          [old-right-br (bnode-right root)])
      (bnode-set-left! root old-right-br)
      (bnode-set-right! root old-left-br)
      root)))

(define [btree-paths root]
  (let* ([left-bnode (bnode-left root)]
         [right-bnode (bnode-right root)]
         [extend (curry cons root)]
         [get-left-paths (λ _ (map extend (btree-paths left-bnode)))]
         [get-right-paths (λ _ (map extend (btree-paths right-bnode)))])
    (match (list left-bnode right-bnode)
      [(list '() '()) (list (list root))]
      [(list '() _) (get-right-paths)]
      [(list _ '()) (get-left-paths)]
      [_ (append (get-left-paths) (get-right-paths))])))

(provide (all-defined-out))
