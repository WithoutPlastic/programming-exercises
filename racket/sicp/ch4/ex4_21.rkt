#lang racket

((lambda [n]
   ((lambda [fact]
            (fact fact n))
    (lambda [ft k]
      (if [= k 1]
        1
        (* k (ft ft (- k 1)))))))
 10)

((lambda [n]
   ((lambda [f] (f f n))
    (lambda [self k]
      (cond ([= k 0] 0)
            ([= k 1] 1)
            (else (+ (self self (- k 1))
                     (self self (- k 2))))))))
 10)

(define [f0 x]
  (define [even? n] (if [= n 0] #t (odd? (- n 1))))
  (define [odd? n] (if [= n 0] #f (even? (- n 1))))
  (even? x))

(define [f x]
  ((lambda [ev? od?]
     (ev? ev? od? x))
   (lambda [ev? od? x]
     (if [= x 0] #t (od? ev? od? (- x 1))))
   (lambda [ev? od? x]
     (if [= x 0] #f (ev? ev? od? (- x 1))))))
