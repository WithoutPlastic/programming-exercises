#lang racket

;Problem:
;There is number sequence a0, a1, ... an-1, length n.
;
;Please write program to figure out the longest ascending sub sequence.
;
;Note: ascending sub sequence, all the element should met requirement: with i < j, ai < aj.
;
;Restriction:
;1 <= n <= 1000
;0 <= ai <= 1000000


(require "../../lib/memorize-function.rkt")


(define [find-longest-ascending-sub-sequence n-lst]
  (define [iter n-lst counter last-n]
    (cond ([null? n-lst] counter)
          ([< last-n (car n-lst)] (iter (cdr n-lst) (add1 counter) (car n-lst)))
          (else (max (iter (cdr n-lst) counter last-n)
                     (iter (cdr n-lst) 1 (car n-lst))))))

  ((memorize-func iter) (cdr n-lst) 1 (car n-lst)))


(find-longest-ascending-sub-sequence (list 4 2 3 1 5))
