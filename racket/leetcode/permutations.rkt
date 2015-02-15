#lang racket

;Problem:
;Given a collection of numbers, return all possible permutations.
;
;For example,
;[1,2,3] have the following permutations:
;[1,2,3], [1,3,2], [2,1,3], [2,3,1], [3,1,2], and [3,2,1].

(define [all-permutations elts]
  (cond ([null? elts] '())
        ([= 1 (length elts)] (list elts))
        (else
          (append-map
            (lambda [elt]
              (map (curry cons elt)
                   (all-permutations (filter-not (curry eq? elt) elts))))
            elts))))

(define test-ints-a '(1 2 3))
(define test-ints-b '(1 2 3 4))

(all-permutations test-ints-a)
(all-permutations test-ints-b)
