#lang racket

;Problem:
;Given an array of integers, every element appears three times except for one.
;Find that single one.
;
;Note:
;Your algorithm should have a linear runtime complexity. Could you implement it
;without using extra memory?

(define [integer->32bit-list int]
  (map (λ [s] (if [bitwise-bit-set? int s] 1 0)) (range 0 32)))

(define [single-number ints]
  (foldl + 0 (map * (map (curry expt 2) (range 0 32))
                    (map (curryr remainder 3)
                         (foldl (λ [n r] (map + (integer->32bit-list n) r))
                                (make-list 32 0) ints)))))

(single-number '(1 4 6 7 7 6 9 4 1 4 1 7 6))
