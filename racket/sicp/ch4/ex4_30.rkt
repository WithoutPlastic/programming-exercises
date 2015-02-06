#lang racket

;Answer:
;<a> As my evalutor, procedure is splited into primitive and compound
;procedure. and primitive is never delayed its arguments evaluation. So nothing
;changed.
;
;<b> The e in inner p procedure nested in p2 is never referenced by others. So
;the argument expr is never evaluated. But the Cy's strategy is different, all
;sequence expr if force evaluated.
;
;original eval-sequence:
;(p1 1) -> (1 . 2)
;(p2 1) -> 1
;
;Cy's eval-sequence:
;(p1 1) -> (1 . 2)
;(p2 1) -> (1 . 2)
;
;<c> Because primitive procedure.
;<d> I agree with original strategy. Sequence expr evaluation's result depends
;on it inner dependency, or in other word, side effect. Programmer is better to
;put side-effects expr in the sequence. Because of side effects, those exprs
;have sequence, sequence make sense. Those non side effect expr is dropped by
;lazy evaluation strategy is acceptable
