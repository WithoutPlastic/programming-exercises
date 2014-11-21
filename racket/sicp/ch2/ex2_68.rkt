#lang racket

(define [make-leaf symbol weight] (list 'leaf symbol weight))
(define [leaf? item] (eq? (car item) 'leaf))
(define [leaf-symbol leaf] (cadr leaf))
(define [leaf-weight leaf] (caddr leaf))

(define [make-code-tree left-tree right-tree]
  (list left-tree
        right-tree
        (append (symbols left-tree) (symbols right-tree))
        (+ (weight left-tree) (weight right-tree))))
(define [tree-left tree] (car tree))
(define [tree-right tree] (cadr tree))
(define [tree-symbols tree] (caddr tree))
(define [tree-weight tree] (cadddr tree))

(define [symbols item]
  (if [leaf? item]
    (list (leaf-symbol item))
    (tree-symbols item)))

(define [weight item]
  (if [leaf? item]
    (leaf-weight item)
    (tree-weight item)))

(define [decode input-bits code-tree]
  (define [bit-decode left-bits current-branch]
    (if [null? left-bits]
      '()
      (let ([next-branch (select-branch-via-bit (car left-bits) current-branch)])
        (if [leaf? next-branch]
          (cons (leaf-symbol next-branch)
                (bit-decode (cdr left-bits) code-tree))
          (bit-decode (cdr left-bits) next-branch)))))
  (bit-decode input-bits code-tree))

(define [select-branch-via-bit bit tree]
  (cond ([= bit 0] (tree-left tree))
        ([= bit 1] (tree-right tree))
        (else (error "bad bit -- SELECT-BRANCH-VIA-BIT" bit))))

(define [encode input-symbols code-tree]
  (define [encode-symbol left-symbols current-branch]
    (cond ([null? left-symbols] '())
          ([leaf? current-branch]
           (encode-symbol (cdr left-symbols) code-tree))
          ([member (car left-symbols) (symbols (tree-left current-branch))]
           (cons 0 (encode-symbol left-symbols (tree-left current-branch))))
          ([member (car left-symbols) (symbols (tree-right current-branch))]
           (cons 1 (encode-symbol left-symbols (tree-right current-branch))))
          (else (error "bad symbol -- ENCODE-SYMBOLS" (car left-symbols)))))
  (encode-symbol input-symbols code-tree))

(define [adjoin-set element set]
  (cond ([null? set] (list element))
        ([< (weight element) (weight (car set))] (cons element set))
        ([< (weight (car set)) (weight element)]
         (cons (car set)
               (adjoin-set element (cdr set))))))

(define [make-leaf-set pairs]
  (if [null? pairs]
    '()
    (let ([pair (car pairs)])
      (adjoin-set (make-leaf (car pair)
                             (cadr pair))
                  (make-leaf-set (cdr pairs))))))

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                    (make-leaf 'B 2)
                    (make-code-tree
                      (make-leaf 'D 1)
                      (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
(decode sample-message sample-tree)
(encode (decode sample-message sample-tree) sample-tree)
