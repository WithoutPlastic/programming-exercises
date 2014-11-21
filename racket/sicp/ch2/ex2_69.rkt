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
  (if [null? input-symbols]
    '()
    (append (encode-symbol (car input-symbols) code-tree)
            (encode (cdr input-symbols) code-tree))))

(define [encode-symbol symbol code-tree]
  (cond ([leaf? code-tree] '())
        ([member symbol (symbols (tree-left code-tree))]
         (cons 0 (encode-symbol symbol (tree-left code-tree))))
        ([member symbol (symbols (tree-right code-tree))]
         (cons 1 (encode-symbol symbol (tree-right code-tree))))
        (else (error "bad symbol -- ENCODE-SYMBOL" symbol))))

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

(define [generate-huffman-code-tree symbol-weight-pairs]
  (define [merge leaf-set]
    (if [= (length leaf-set)]
      (car leaf-set)
      (merge (adjoin-set (make-code-tree (cadr leaf-set)
                                         (car leaf-set))
                         (cddr leaf-set)))))
  (merge (make-leaf-set symbol-weight-pairs)))

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

