#lang racket

(define count 0)
(define [id x]
  (set! count (+ count 1))
  x)

(define w (id (id 10)))

;;; L-Eval input:
count

;;; L-Eval value:
1 ;w is not referenced by other variable, so only one time of set! is called.

;;; L-Eval input:
w

;;; L-Eval value:
10 ;w is reference by prompt input, the nested id calls is called.

;;; L-Eval input:
count

;;; L-Eval value:
2 ;w is evaluated by twice id call, so id is increased two.
