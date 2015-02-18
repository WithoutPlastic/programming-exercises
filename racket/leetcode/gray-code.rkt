#lang racket

;Problem:
;The gray code is a binary numeral system where two successive values differ in
;only one bit.
;
;Given a non-negative integer n representing the total number of bits in the
;code, print the sequence of gray code. A gray code sequence must begin with 0.
;
;For example, given n = 2, return [0,1,3,2]. Its gray code sequence is:
;
;00 - 0
;01 - 1
;11 - 3
;10 - 2
;
;Note:
;For a given n, a gray code sequence is not uniquely defined.
;
;For example, [0,2,3,1] is also a valid gray code sequence according to the
;above definition.

(define [gray-code n]
  (define [iter r result]
    (define [continue]
      (iter (add1 r)
            (append-map (lambda [ret]
                          (map (lambda [x] (+ ret (if [= x 1] (expt 2 r) 0)))
                               (range 0 2)))
                        result)))

    (if [< r n] (continue) result))

  (iter 0 (list 0)))

(gray-code 2)
(gray-code 3)
(gray-code 4)
