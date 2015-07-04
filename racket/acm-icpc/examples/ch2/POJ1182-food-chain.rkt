#lang racket

;Problem(POJ 3617):
;There is N animal, with serial number 1, 2, ... N. All animal be one kind of (A, B, C). And we
;know food chain is B => A, C => B, A => C.
;
;With sequence two type statement below:
;- x and y to be one same kind of animal.
;- y => x.
;
;But the statement maybe incorrect. And also, the serial number is not included.
;Please write program, with K statement input, return how many statement is incorrect.
;Ignore incorrect error message.
;
;Restriction:
;1 <= N <= 50000
;0 <= K <= 100000


;(require data/union-find)
(require "../../lib/union-find-set.rkt")


(define statement-type-i 'statement-type-i)
(define statement-type-ii 'statement-type-ii)


(define [calculate-incorrect-statement statement-lst N]
  (let* ([init-union-find-set-A (map uf-new (range 1 (add1 N)))]
         [init-union-find-set-B (map uf-new (range 1 (add1 N)))]
         [init-union-find-set-C (map uf-new (range 1 (add1 N)))]
         [idx-valid? (λ [idx] [and [<= 0 idx] [< idx N]])]
         [serial-number-valid? (λ [statement] [and [idx-valid? (sub1 (cadr statement))] 
                                                   [idx-valid? (sub1 (caddr statement))]])]
         [illegal-idx-counter (count (negate serial-number-valid?) statement-lst)]
         [valid-idx-statement-lst (filter serial-number-valid? statement-lst)])
    (let iter ([incorrect-counter illegal-idx-counter]
               [remaining-statement-lst valid-idx-statement-lst])
      (if [null? remaining-statement-lst] incorrect-counter
        (let* ([cur-statement (car remaining-statement-lst)]
               [rest-statement-lst (cdr remaining-statement-lst)]
               [cur-statement-type (car cur-statement)]
               [x-idx (sub1 (cadr cur-statement))]
               [y-idx (sub1 (caddr cur-statement))]
               [uf-set-x-A (list-ref init-union-find-set-A x-idx)]
               [uf-set-x-B (list-ref init-union-find-set-B x-idx)]
               [uf-set-x-C (list-ref init-union-find-set-C x-idx)]
               [uf-set-y-A (list-ref init-union-find-set-A y-idx)]
               [uf-set-y-B (list-ref init-union-find-set-B y-idx)]
               [uf-set-y-C (list-ref init-union-find-set-C y-idx)])
          (if [eq? cur-statement-type statement-type-i]
            (if [or [uf-same-set? uf-set-x-A uf-set-y-B] [uf-same-set? uf-set-x-A uf-set-y-C]]
              (iter (add1 incorrect-counter) (cdr remaining-statement-lst))
              (begin
                (uf-union! uf-set-x-A uf-set-y-A)
                (uf-union! uf-set-x-B uf-set-y-B)
                (uf-union! uf-set-x-C uf-set-y-C)
                (iter incorrect-counter (cdr remaining-statement-lst))))
            (if [or [uf-same-set? uf-set-x-A uf-set-y-A]
                    [uf-same-set? uf-set-x-A uf-set-y-C]]
              (iter (add1 incorrect-counter) (cdr remaining-statement-lst))
              (begin
                (uf-union! uf-set-x-A uf-set-y-B)
                (uf-union! uf-set-x-B uf-set-y-C)
                (uf-union! uf-set-x-C uf-set-y-A)
                (iter incorrect-counter (cdr remaining-statement-lst))))))))))


(calculate-incorrect-statement
  (list (list statement-type-i 101 1)
        (list statement-type-ii 1 2)
        (list statement-type-ii 2 3)
        (list statement-type-ii 3 3)
        (list statement-type-i 1 3)
        (list statement-type-ii 3 1)
        (list statement-type-i 5 5))
  100)
