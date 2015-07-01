#lang racket

;Problem:
;Given an list of n integers, find four numbers k such that they add up to a specific target
;number.
;
;Note: the integers can be taken more than once.
;
;Restriction:
;4 <= n <= 1000
;0 <= k <= 1000000

(define [pick-n-elts lst n]
  (cond ([= n 0] (list '()))
        ([or [< n 0] [< (length lst) n]] '())
        (else (append (map (curry cons (car lst)) (pick-n-elts lst (- n 1)))
                      (pick-n-elts (cdr lst) n)))))

(define [binary-search lst n]
  (let* ([len (length lst)]
         [mid-idx (floor (/ len 2))]
         [mid-n (list-ref lst mid-idx)])
    (cond ([= len 1] [= (car lst) n])
          ([= len 2] [or [= (car lst) n] [= (cadr lst) n]])
          ([< n mid-n] (binary-search (take lst mid-idx) n))
          ([< mid-n n] (binary-search (drop lst (add1 mid-idx)) n))
          (else true))))

(define [four-sum int-list target-sum]
  (let* ([unique-int-list (remove-duplicates int-list)]
         [two-permutation (pick-n-elts unique-int-list 2)]
         [two-sum-permutation (map (curry apply +) two-permutation)]
         [unique-two-sum-lst (remove-duplicates two-sum-permutation)]
         [ascending-two-sum-lst (sort unique-two-sum-lst <)])
    [ormap (λ [two-sum] (binary-search two-sum-permutation (- target-sum two-sum)))
           two-sum-permutation]))

(four-sum (list 2 3 4 56 8 1 2 1 8 3) 24)
(four-sum (list 2 3 4 56 8 1 2 1 8 3) 25)
