#lang racket

(let ([pairs empty])
  (if-fail
    (let ([p (prime-sum-pair '(1 3 5 8) '(20 35 110))])
      (permanent-set! pairs (cons p pairs))
      (amb))
    pairs))

;Answer: ((8 35) (3 110) (3 20))
