#lang racket

(rule (append-to-form () 'x 'x))
(rule (append-to-form (?u . ?v) ?y (?u . ?z))
      (append-to-form ?v ?y ?z))

(rule (reverse () ()))
(rule (reverse ?x ?y)
      (and (append-to-form (?first) ?rest ?x)
           (append-to-form ?rev-rest (?first) ?y)
           (reverse ?rest ?rev-rest)))

(reverse (1 2 3) ?x); infinite loop
(reverse ?x (1 2 3))

;Answer: Something really confusing me when I see different rule solutions. and
;the execution results. In above one, one reverse query dropped in infinite
;loop. The rule body expansion is symmetrical. So follow intuition, both reverse
;query should work. What happened? The evaluation sequence matters.
;
;In above demo, (reverse (1 2 3) ?x) go to (append-to-form ?rev-rest (?first) ?y)
;the potential solutions is not limited(?first known, but both ?rev-rest and ?y
;not). Whole procedure dropped into infinite loop.
;
;If we swap the last reverse expression and it, (reverse (1 2 3) ?x) work well.
;But (reverse ?x (1 2 3)) dropped into infinite loop. Because both reverse
;arguments is non-determinal.
;
;One conclusion is that, the reverse query have direction, not following the
;direction, you won't get right result. And it is impossible to find a reverse
;rule to do bidirection reverse(not sure, without case-by-case detector).
