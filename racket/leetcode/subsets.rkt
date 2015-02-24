#lang racket

;Problem:
;Given a set of distinct integers, S, return all possible subsets.
;
;Note:
;
;Elements in a subset must be in non-descending order.
;The solution set must not contain duplicate subsets.
;
;For example,
;If S = [1,2,3], a solution is:
;
;[
;[3],
;[1],
;[2],
;[1,2,3],
;[1,3],
;[2,3],
;[1,2],
;[]
;]

(define [n-permute lst n]
  (let ([len (length lst)])
    (cond ([= n 0] (list '()))
          ([= n 1] (map list lst))
          ([= n len] (list lst))
          (else (append (map (λ [x] (cons (car lst) x))
                             (n-permute (cdr lst) (sub1 n)))
                        (n-permute (cdr lst) n))))))

(define [subsets ints]
  (let* ([sorted-ints (sort ints <)] [len (length ints)]
         [result-lens (range 0 (add1 len))])
    (append-map (curry n-permute sorted-ints) result-lens)))

(define test-set '(1 2 3))

(subsets test-set)
