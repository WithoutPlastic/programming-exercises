#lang racket

;Problem:
;Find out n numbers that their sum equal to given target.
;Numbers should not be used more than once.

(define [n-sum int-lst n target-sum]
  (let iter ([remaining-int-lst int-lst]
             [remaining-cnt n]
             [current-sum 0])
    (cond ([< remaining-cnt 0] false)
          ([= remaining-cnt 0] [= current-sum target-sum])
          ([< (length remaining-int-lst) remaining-cnt] false)
          (else [or (iter (cdr remaining-int-lst)
                          remaining-cnt
                          current-sum)
                    (iter (cdr remaining-int-lst)
                          (sub1 remaining-cnt)
                          (+ current-sum (car remaining-int-lst)))]))))

(n-sum (list 2 3 4 56 8 1 2 1 8 3) 4 23)
(n-sum (list 2 3 4 56 8 1 2 1 8 3) 4 24)
