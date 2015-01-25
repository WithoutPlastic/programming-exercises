#lang racket

(define [flatmap proc items] (foldr append null (map proc items)))

(define [rank-permutations available-set]
  (if [null? available-set]
    (list null)
    (flatmap (lambda [x] (map (lambda [y] (cons x y))
                              (rank-permutations (remove x available-set))))
             available-set)))

(define [format-solution solution-list]
  (map list '(betty ethel john kitty mary) solution-list))

(define [xor a b] [or [and a [not b]] [and [not a] b]])
(define [valid-solution? permutation]
  [and [xor [= (fourth permutation) 2] [= (first permutation) 3]]
       [xor [= (second permutation) 1] [= (third permutation) 2]]
       [xor [= (third permutation) 3] [= (second permutation) 5]]
       [xor [= (fourth permutation) 2] [= (fifth permutation) 4]]
       [xor [= (fifth permutation) 4] [= (first permutation) 1]]])

(define [multi-ranking]
  (map format-solution
       (filter valid-solution? (rank-permutations (list 1 2 3 4 5)))))
  
(multi-ranking)
