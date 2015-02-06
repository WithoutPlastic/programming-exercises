#lang racket

(rule (last-pair (?x) (?x)))
(rule (last-pair (?u . ?v) (?x))
      (last-pair ?v (?x)))

;< (last-pair (3) ?x)
;> (last-pair (3) (3))
;< (last-pair (1 2 3) ?x)
;> (last-pair (1 2 3) (3))
;< (last-pair (2 ?x) (3))
;> (last-pair (2 3) (3))

;Answer: there is infinite combinations of (last-pair ?x (3)), evaluator should
;given no answer or a hint for the result.
