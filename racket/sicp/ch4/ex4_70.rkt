#lang racket

;Answer: This let expr matter because of delay/force. And it is wired when do
;assignment.
;
;I jump over this issue when rewrite query evaluator. Because orignal bindings
;is not delay stream structure. But I forgot change it when introducing lazy
;stream.
;
;In first case, in procedure call, arguments get evaluated. It should be the
;same result using let expr. Both indeed is a procedure call.
;
;In latter case, it will do parallel assignment, and without let expr, set! will
;constructing self-pointing data strcuture as we see. A let expr can avoiding
;this annoying problem.

;Welcome to Racket v5.3.6.
;>
;> (define x (cons 1 2))
;> x
;'(1 . 2)
;> (set! x (cons 0 x))
;> x
;'(0 1 . 2)
;> (set! x (cons 0 (delay x)))
;> x
;'(0 . #<promise:stdin::64>)
;> x
;'(0 . #<promise:stdin::64>)
;> (cdr x)
;#<promise:stdin::64>
;> (force (cdr x))
;#0='(0 . #<promise!#0#>)
;>
;
