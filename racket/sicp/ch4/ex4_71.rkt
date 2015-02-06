#lang racket

;Answer: This problam is ignore when rewrite query evaluator. I change the
;stream-append and stream-interleave procedure with normal stream parameters.
;And this is more intutive! Half delay parameter is somewhat wired to me. But
;with top view, half way delay is more lazy. Obviously, it handle those case may
;caught infinite loop.
;
;As you can see, the affected qeval procedure simple-qeval/disjunction-qeval,
;they both have combination logic. So half way delay works when those infinite
;loop lay on second expression.
;
;With other online answer for this exercise, the benefit is more informative to
;end user.
