#lang racket

(define (square x) (* x x))

(define (smallest-divisor n) (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b) (= (remainder b a) 0))

(define (prime? n) (= n (smallest-divisor n)))

(define (timed-prime-test n) (start-prime-test n (current-milliseconds)))

(define (start-prime-test n start-time)
  (when (prime? n) (report-prime n (- (current-milliseconds) start-time))))

(define (report-prime n elapsed-time)
  (begin
    (newline)
    (display n)
    (display " *** ")
    (display elapsed-time)))

(define (search-for-primes first last)
  (define (search-iter cur last)
    (when (< cur last)
      (begin
        (timed-prime-test cur))
        (search-iter (+ cur 2) last)))
  (search-iter (if (even? first) (+ first 1) first)
               (if (even? last) (- last 1) last)))

(search-for-primes 1000 1019)
(search-for-primes 10000 10037)
(search-for-primes 100000 100043)
(search-for-primes 1000000 1000037)
(search-for-primes 1000000000 1000000021)
(search-for-primes 10000000000 10000000067)
(search-for-primes 100000000000 100000000057)
(search-for-primes 1000000000000 1000000000063)
