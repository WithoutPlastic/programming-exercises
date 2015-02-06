#lang racket

(define [distinct? items]
  (cond ([null? items] true)
        ([null? (cdr items)] true)
        ([member (car items) (cdr items)] false)
        (else (distinct? (cdr items)))))

(define [format-solution solution-list]
  (map list '(betty ethel john kitty mary) solution-list))
(define [amb-liars]
  (let ([betty (amb 1 2 3 4 5)]
        [ethel (amb 1 2 3 4 5)]
        [john (amb 1 2 3 4 5)]
        [kitty (amb 1 2 3 4 5)]
        [mary (amb 1 2 3 4 5)])
    (require [and [xor [= kitty 2] [= betty 3]]
                  [xor [= ethel 1] [= john 2]]
                  [xor [= john 3] [= ethel 5]]
                  [xor [= kitty 2] [= mary 4]]
                  [xor [= mary 4] [= betty 1]]
                  [distinct? (list betty ethel john kitty mary)]])
    (format-solution (list betty ethel john kitty mary))))
;(define [flatmap proc items] (foldr append null (map proc items)))
;
;(define [rank-permutations available-set]
;  (if [null? available-set]
;    (list null)
;    (flatmap (lambda [x] (map (lambda [y] (cons x y))
;                              (rank-permutations (remove x available-set))))
;             available-set)))
;
;(define [format-solution solution-list]
;  (map list '(betty ethel john kitty mary) solution-list))
;
;(define [xor a b] [or [and a [not b]] [and [not a] b]])
;(define [valid-solution? permutation]
;  [and [xor [= (fourth permutation) 2] [= (first permutation) 3]]
;       [xor [= (second permutation) 1] [= (third permutation) 2]]
;       [xor [= (third permutation) 3] [= (second permutation) 5]]
;       [xor [= (fourth permutation) 2] [= (fifth permutation) 4]]
;       [xor [= (fifth permutation) 4] [= (first permutation) 1]]])
;
;(define [multi-ranking]
;  (map format-solution
;       (filter valid-solution? (rank-permutations (list 1 2 3 4 5)))))
;  
;(multi-ranking)
