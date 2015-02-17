#lang racket

;Problem:
;You are climbing a stair case. It takes n steps to reach to the top.
;
;Each time you can either climb 1 or 2 steps. In how many distinct ways can you
;climb to the top? 

(define [climbing-stairs n]
  (cond ([< 2 n] (+ (climbing-stairs (- n 1)) (climbing-stairs (- n 2))))
        ([= n 2] 2)
        ([= n 1] 1)
        (else 0)))

(climbing-stairs 3)
(climbing-stairs 4)
(climbing-stairs 5)
(climbing-stairs 6)
(climbing-stairs 7)
