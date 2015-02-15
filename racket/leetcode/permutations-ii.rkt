#lang racket

;Problem:
;Given a collection of numbers that might contain duplicates, return all
;possible unique permutations.
;
;For example,
;[1,1,2] have the following unique permutations:
;[1,1,2], [1,2,1], and [2,1,1].

(define [permute-unique elts]
  (let ([uniques (remove-duplicates elts)])
    (cond ([null? elts] '())
          ([= 1 (length elts)] (list elts))
          (else
            (append-map
              (lambda [elt]
                (map (curry cons elt) (permute-unique (remove elt elts))))
              uniques)))))

(define test-ints '(1 1 2))

(permute-unique test-ints)
