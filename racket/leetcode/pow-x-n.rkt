#lang racket

;Problem:
;Implement pow(x, n).

(define [pow x n]
  (cond ([= n 0] 1)
        ([< 0 n] (* x (pow x (sub1 n))))
        (else (* (/ 1 x) (pow x (add1 n))))))

(pow 2 10)
(pow 3 3)
(pow 3 -3)
(pow -3 3)
(pow -3 -3)
