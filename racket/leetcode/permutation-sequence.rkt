#lang racket

;Problem:
;The set [1,2,3,…,n] contains a total of n! unique permutations.
;
;By listing and labeling all of the permutations in order,
;We get the following sequence (ie, for n = 3):
;
;"123"
;"132"
;"213"
;"231"
;"312"
;"321"
;
;Given n and k, return the kth permutation sequence.
;
;Note: Given n will be between 1 and 9 inclusive.

(require "lib/permutation.rkt")

(define [get-permutation n k]
  (define [factorial n] (if [< 1 n] (* n (factorial (sub1 n))) 1))

  (let ([permutations-len (factorial n)]
        [permutations (unique-permute (range 1 (add1 n)))])
    (if [<= k permutations-len]
      (list-ref permutations (sub1 k))
      (list-ref permutations (sub1 permutations-len)))))

(get-permutation 3 5)
