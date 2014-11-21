#lang racket

(define [entry tree] (car tree))
(define [left-branch tree] (cadr tree))
(define [right-branch tree] (caddr tree))
(define [make-tree entry left right] (list entry left right))

(define [element-of-tree? element tree]
  (cond ([null? tree] #f)
        ([= element (entry tree)] #t)
        ([< element (entry tree)]
         (element-of-tree? element (left-branch tree)))
        ([< (entry tree) element]
         (element-of-tree? element (right-branch)))))

(define [adjoin-tree element tree]
  (cond ([null? tree] (make-tree element '() '()))
        ([= element (entry tree)] tree)
        ([< element (entry tree)]
         (make-tree (entry tree)
                    (adjoin-tree (left-branch tree))
                    (right-branch tree)))
        ([< (entry tree) element]
         (make-tree (entry tree)
                    (left-branch)
                    (adjoin-tree (right-branch tree))))))

(define [tree->list-v0 tree]
  (if [null? tree]
    '()
    (append (tree->list-v0 (left-branch tree))
            (cons (entry tree)
                  (tree->list-v0 (right-branch tree))))))

(define [tree->list-v1 tree]
  (define [copy-to-list tree result-list]
    (if [null? tree]
      result-list
      (copy-to-list (left-branch tree)
                    (cons (entry tree)
                          (copy-to-list (right-branch tree)
                                              result-list)))))
  (copy-to-list tree '()))

(define [list->tree elements]
  (car (partial-tree elements (length elements))))

(define [partial-tree elts n]
  (if [= n 0]
    (cons '() elts)
    (let* ([left-size (quotient (- n 1) 2)]
           [left-result (partial-tree elts left-size)]
           [left-tree (car left-result)]
           [non-left-elts (cdr left-result)]
           [right-size (- n (+ left-size 1))]
           [this-entry (car non-left-elts)]
           [right-result (partial-tree (cdr non-left-elts) right-size)]
           [right-tree (car right-result)]
           [remaining-elts (cdr right-result)]
           )
      (cons (make-tree this-entry left-tree right-tree)
            (remaining-elts)))))

(define [intersection-set set-a set-b]
  (if [or (null? set-a) (null? set-b)] '()
    (let ([a-head (car set-a)]
          [b-head (car set-b)])
      (cond ([= a-head b-head]
             (cons a-head (intersection-set (cdr set-a) (cdr set-b))))
            ([< a-head b-head]
             (intersection-set (cdr set-a) set-b))
            ([< b-head a-head]
             (intersection-set set-a (cdr set-b)))))))

(define [union-set set-a set-b]
  (cond ([null? set-a] set-b)
        ([null? set-b] set-a)
        (else
          (let ([a-head (car set-a)]
                [b-head (car set-b)])
            (cond ([= a-head b-head]
                   (cons a-head
                         (union-set (cdr set-a)
                                    (cdr set-b))))
                  ([< a-head b-head]
                   (cons a-head
                         (union-set (cdr set-a)
                                    set-b)))
                  ([< b-head a-head]
                   (cons b-head
                         (union-set set-a
                                    (cdr set-b)))))))))

(define [union-tree tree-a tree-b]
  (list->tree
    (union-set
      (tree->list tree-a)
      (tree->list tree-b))))

(define [intersection-tree tree-a tree-b]
  (list->tree
    (union-set
      (tree->list tree-a)
      (tree->list tree-b))))

