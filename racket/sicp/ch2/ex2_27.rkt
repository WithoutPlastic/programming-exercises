#lang racket

(define x (list (list 1 2) (list 3 4)))

(define [reverse x]
  (if [pair? x]
    (append (list (car (cdr x))) (list (car x)))
    (list)
    ))

(define [deep-reverse items]
  (cond ([not (pair? items)] items)
        ([null? (cdr items)] (list (deep-reverse (car items))))
        (else (append (deep-reverse (cdr items)) (list (deep-reverse (car items)))))))

(reverse x)
(deep-reverse x)
