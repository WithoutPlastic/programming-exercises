#lang racket

(define [element-of-set? element set]
  (cond ([null? set] #f)
        ([equal? element (car set)] #t)
        (else (element-of-set? element (cdr set)))))
(define [adjoin-set element set]
  (if [element-of-set? element set] set (cons element set)))

(define [intersection-set set-a set-b]
  (cond ([or (null? set-a) (null? set-b)] '())
        ([element-of-set? (car set-a) set-b]
         (cons (car set-a)
               (intersection-set (cdr set-a) set-b)))
        (else (intersection-set (cdr set-a) set-b))))

(define [union-set set-a set-b]
  (cond ([null? set-a] set-b)
        ([null? set-b] set-a)
        ([element-of-set? (car set-a) set-b]
         (union-set (cdr set-a) set-b))
        (else (union-set (cdr set-a) (cons (car set-a) set-b)))))
