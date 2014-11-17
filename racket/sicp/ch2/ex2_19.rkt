#lang racket

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define [first-denomination coins-values] (car coins-values))
(define [except-first-denomination coins-values] (cdr coins-values))
(define [no-more? coins-values] (null? coins-values))
(define [cc amount coins-values]
  (cond
    ([= amount 0] 1)
    ([or (< amount 0) (no-more? coins-values)] 0)
    (else
      (+
        (cc amount
            (except-first-denomination coins-values))
        (cc (- amount
               (first-denomination coins-values))
            coins-values)))))

(cc 100 us-coins)
