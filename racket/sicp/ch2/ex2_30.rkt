#lang racket

(define [square-tree-using-map tree]
  (map
    (lambda (sub-tree)
      (if [pair? sub-tree]
        (square-tree-using-map sub-tree)
        (expt sub-tree 2)))
    tree))

(define [square-tree tree]
  (cond 
    ([null? tree] '())
    ([number? tree] (expt tree 2))
    ([and (number? (car tree)) (null? (cdr tree))] (expt (car tree) 2))
    ([pair? tree] (cons (square-tree (car tree)) (square-tree (cdr tree))))))

(square-tree-using-map (list 1 (list 2 (list 3 4) 5) (list 6 7)))
(square-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))
