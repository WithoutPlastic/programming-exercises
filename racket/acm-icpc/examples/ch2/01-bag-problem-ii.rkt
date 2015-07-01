#lang racket

;Problem:
;There is n goods, each weight Wi, value Vi. Pick up goods with their total weight less than W.
;Wirte a program to calculate max value under requirement.
;
;Restriction:
;1 <= n <= 100
;1 <= Wi <= 10^7
;1 <= Vi <= 100
;1 <= W <= 10^9


(require "../../lib/memorize-function.rkt")


(define [find-elt-index-right lst pred?]
  (let ([reversed-lst (reverse lst)])
    (let iter ([remaining reversed-lst] [counter 1])
      (cond ([null? remaining] false)
            ([pred? (car remaining)] (- (length lst) counter))
            (else (iter (cdr remaining) (add1 counter)))))))


(define [get-min-weight-combination goods-lst max-value]
  (define [iter weight-lst value-lst max-value]
    (cond ([null? value-lst] (if [= max-value 0] 0 +inf.0))
          ([< max-value (car value-lst)] (iter (cdr weight-lst) (cdr value-lst) max-value))
          (else (min (iter (cdr weight-lst) (cdr value-lst) max-value)
                     (+ (car weight-lst)
                        (iter (cdr weight-lst) (cdr value-lst) (- max-value (car value-lst))))))))

  (let ([weight-lst (map car goods-lst)] [value-lst (map cdr goods-lst)])
    (if [or [null? goods-lst] [< max-value (apply min value-lst)]] +inf.0
      ((memorize-func iter) weight-lst value-lst max-value))))


(define [get-max-value-combination goods-lst max-weight]
  (let ([goods-count (length goods-lst)])
    (let iter-value ([value-limit 0] [result-lst '()])
      (if [< (* goods-count (apply max (map cdr goods-lst))) value-limit]
        (find-elt-index-right result-lst (curryr <= max-weight))
        (iter-value
          (add1 value-limit)
          (append result-lst (list (get-min-weight-combination goods-lst value-limit))))))))


(get-max-value-combination (list (cons 2 3) (cons 1 2) (cons 3 4) (cons 2 2)) 5)
