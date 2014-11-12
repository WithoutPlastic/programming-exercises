#lang racket

(define [square x] (* x x))

(define [expmod base exp mod]
  (cond ((zero? exp) 1)
        ((even? exp) (remainder (square (expmod base (/ exp 2) mod)) mod))
        (else (remainder (* base (expmod base (- exp 1) mod)) mod))))

(define [extra-square-mod-one-test n a]
  (or (= a (- n 1)) (= a 1) (not (= (remainder (square a) n) 1))))

(define [miller-rabin-test? n a]
  (and (= (expmod a (- n 1) n) 1)
       (extra-square-mod-one-test n a)))

(define [fully-prime-test n]
  (define [iteration n a]
    (begin 
      (cond ((zero? a) #t)
            ((miller-rabin-test? n a) (iteration n (- a 1)))
            (else #f))))
  (iteration n (- n 1)))

(define [given-times-prime-test? n times]
  (cond ((zero? times) #t)
        ((miller-rabin-test? n (+ 1 (random (- n 1)))) 
         (given-times-prime-test? n (- times 1)))
        (else #f)))

(given-times-prime-test? 561 10)
(fully-prime-test 561)
(fully-prime-test 562)
(fully-prime-test 1105)
(fully-prime-test 1729)
