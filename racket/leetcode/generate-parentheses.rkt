#lang racket

;Problem:
;Given n pairs of parentheses, write a function to generate all combinations of
;well-formed parentheses.
;
;For example, given n = 3, a solution set is:
;
;"((()))", "(()())", "(())()", "()(())", "()()()".

(define op #\() (define cp #\))

(define [repeat-parenthesis n]
  (append (make-list n op) (make-list n cp)))

(define [repeat-prefix n]
  (let ([prefix-range (range 1 n)])
    (apply append
           (map (lambda [r]
                  (map (lambda [x] (append (repeat-parenthesis r) x))
                       (generate-parenthesis (- n r))))
                prefix-range))))

(define [outter-enclosing n]
  (let ([enclosing-repeats (range (- n 2) 0 -1)])
    (apply append
           (map (lambda [r]
                  (map (lambda [x] (append (make-list r op) x (make-list r cp)))
                       (repeat-prefix (- n r))))
                enclosing-repeats))))

(define [generate-parenthesis n]
  (if [< 0 n]
    (append (list (repeat-parenthesis n))
            (repeat-prefix n)
            (outter-enclosing n))
    (list '())))

(map list->string (generate-parenthesis 3))
(map list->string (generate-parenthesis 4))
;(map list->string (generate-parenthesis 5))
