#lang rackek

;Answer: A frequently used case it fib sequence calculation. Because lots of
;duplicated calculate path is performed. With memorization, computing overhead
;is less than non-memorization solution.

(define [square x] (* x x)) ;obvious duplicated evaluation of x
;;; L-Eval input:
(square (id 10))

;;; L-Eval value:
100

;;; L-Eval input:
count

;;; L-Eval value:
1 ;with memorization
2 ;without memorization
