#lang racket

(define [element-of-set? element set]
  (cond ([null? set] #f)
        ([equal? element (car set)] #t)
        (else (element-of-set? element (cdr set)))))
(define [adjoin-set element set]
  (cons element set))

(define [intersection-set set-a set-b]
  (cond ([or (null? set-a) (null? set-b)] '())
        ([element-of-set? (car set-a) set-b]
         (cons (car set-a)
               (intersection-set (cdr set-a) set-b)))
        (else (intersection-set (cdr set-a) set-b))))

(define [union-set set-a set-b]
  (append set-a set-b))
