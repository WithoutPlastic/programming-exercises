#lang racket

;Answer: Because all the result met the prerequirements. Obviously, this don't
;answer the reason very well. Actually, the logic keyword is commutative, so
;result order don't matter. So follow this consideration, we need to made order
;sensative lisp-value. Follow alphabetic is simple idea.
;Rule is not possible, because rule is in the logical domain, whose connector
;like and/or is commutative. Some ability to control underlayer implementation
;is necessary.

(define [person<? person-a person-b]
  (string<? (symbol->string person-a) (symbol->string person-b)))

(assert! (rule (non-duplicated-live-near ?person-a ?person-b)
               (and (address ?person-a (?town . ?foo))
                    (address ?person-b (?town . ?bar))
                    (lisp-value person<? ?person-a ?person-b))))
