#lang racket

;Problem:
;Given a collection of numbers that might contain duplicates, return all
;possible unique permutations.
;
;For example,
;[1,1,2] have the following unique permutations:
;[1,1,2], [1,2,1], and [2,1,1].

(require "lib/permutation.rkt")

(unique-permute '(1 1 2))
