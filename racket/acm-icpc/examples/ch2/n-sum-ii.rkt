#lang racket

;Problem:
;There is n different number Ai, with qty Mi.
;Write a program to verdict there is combination sum equals to K.
;
;Restriction:
;1 <= n <= 100
;1 <= Ai, Mi <= 1000000
;1 <= K <= 1000000


(require "../../lib/memorize-function.rkt")


(define [verdict-n-sum number-pool k]
  (define [iter k selected-num-lst num-lst qty-lst]
    (cond ([= k 0] selected-num-lst)
          ([or [null? num-lst] [< k 0]] false)
          ([or [< k (car num-lst)] [<= (car qty-lst) 0]]
           (iter k selected-num-lst (cdr num-lst) (cdr qty-lst)))
          (else [or (iter k selected-num-lst (cdr num-lst) (cdr qty-lst))
                    (iter (- k (car num-lst))
                          (cons (car num-lst) selected-num-lst)
                          num-lst
                          (cons (sub1 (car qty-lst)) (cdr qty-lst)))])))

  ((memorize-func iter) k '() (map car number-pool) (map cdr number-pool)))


(verdict-n-sum (list (cons 3 3) (cons 5 2) (cons 8 2)) 17)
