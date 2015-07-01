#lang racket

;Problem:
;There is n goods, each weight Wi, value Vi. Pick up goods with their total weight less than W.
;Wirte a program to calculate the max value.
;
;Note: the goods can be pick as many as possible.
;
;Restriction:
;1 <= n <= 100
;1 <= Wi, Vi <= 100
;1 <= W <= 10000


(require "../../lib/memorize-function.rkt")


(define [get-max-value-combination goods-lst max-weight]
  (let ([weight-lst (map car goods-lst)]
        [value-lst (map cdr goods-lst)])
    (cond ([null? goods-lst] 0)
          ([< max-weight (apply max weight-lst)] 0)
          ([< max-weight (car weight-lst)] (get-max-value-combination (cdr goods-lst) max-weight))
          (else (max (get-max-value-combination (cdr goods-lst) max-weight)
                     (+ (car value-lst)
                        (get-max-value-combination goods-lst (- max-weight (car weight-lst)))))))))


((memorize-func get-max-value-combination) (list (cons 3 4) (cons 4 5) (cons 2 3)) 7)
