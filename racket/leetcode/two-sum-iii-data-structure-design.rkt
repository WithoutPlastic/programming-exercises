#lang racket

;Problem:
;Design and implement a TwoSum class. It should support the following
;operations: add and find.
;
;add - Add the number to an internal data structure.
;find - Find if there exists any pair of numbers which sum is equal to the
;value.
;
;For example,
;
;add(1); add(3); add(5);
;find(4) -> true
;find(7) -> false

(require "lib/permutation.rkt")

(define two-sum% (class object%
  (super-new)
  (define ints '())
  (define/public [add! int] (set! ints (cons int ints)))
  (define/public [find sum]
    (if (findf (λ [p] [= (+ (car p) (cadr p)) sum]) (pick-n-elts ints 2))
      true false))))

(let ([two-sum (new two-sum%)])
  (send two-sum add! 1)
  (send two-sum add! 3)
  (send two-sum add! 5)
  (displayln (send two-sum find 4))
  (displayln (send two-sum find 7)))
