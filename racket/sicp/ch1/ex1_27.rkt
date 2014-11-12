#lang racket

(define [square x] (* x x))

(define [expmod base exp m]
  (cond ((= exp 0) 1)
        ((even? exp) (remainder (square (expmod base (/ exp 2) m)) m))
        (else (remainder (* base (expmod base (- exp 1) m)) m))))

(define [fermat-check? n a]
  (= (expmod a n n) a))

(define [carmichael-check n]
  (define [iter n a]
    (cond ((= 1 a) #t)
          ((not (fermat-check? n a)) #f)
          (else (iter n (- a 1)))))
  (iter n (- n 1)))
;  (iter n (round (sqrt n))))

(carmichael-check 561)
(carmichael-check 564)
(carmichael-check 1105)
(carmichael-check 1729)
