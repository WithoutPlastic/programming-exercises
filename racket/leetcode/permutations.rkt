#lang racket

;Problem:
;Given a collection of numbers, return all possible permutations.
;
;For example,
;[1,2,3] have the following permutations:
;[1,2,3], [1,3,2], [2,1,3], [2,3,1], [3,1,2], and [3,2,1].

(require "lib/permutation.rkt")

(define all-permute permute)

(all-permute '(1 2 3))
(all-permute '(1 1 3))
(all-permute '(1 2 3 4))
