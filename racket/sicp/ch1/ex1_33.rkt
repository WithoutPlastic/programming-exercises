#lang racket

(define [prime? x] x)

(define [accumulator-with-filter
          filter?
          combiner
          calculator
          from
          to
          next
          finished?
          finalized-value]
  (define [iter current]
    (cond ([finished? current to] finalized-value)
          ([filter? current] (iter (next current)))
          (else (combiner (calculator current ) (iter (next current))))))
  (iter from))

(define [accumulator combiner calculator from to next finished? finalized-value]
  (accumulator-with-filter
    (lambda (x) #t)
    combiner
    calculator
    from
    to
    next
    finished?
    finalized-value))

(define [prime-accumulator
          combiner
          calculator
          from
          to
          next
          finished?
          finalized-value]
  (accumulator-with-filter
    prime?
    combiner
    calculator
    from
    to
    next
    finished?
    finalized-value))

(define [sum-primes from to]
  (accumulator-with-filter
    prime?
    +
    (lambda (x) x)
    from
    to
    (lambda (x) (+ x 1))
    >=
    0))

(define [product-gcd-prime n]
  (accumulator-with-filter
    gcd-eq?
    *
    (lambda (x) x)
    1
    n
    (lambda (x) (+ x 1))
    >=
    1))
