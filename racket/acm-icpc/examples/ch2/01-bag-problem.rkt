#lang racket

;Problem:
;There is n goods, each weight Wi, value Vi. Pick up goods with their total weight less than W.
;Wirte a program to calculate the max value.
;
;Restriction:
;1 <= n <= 100
;1 <= Wi, Vi <= 100
;1 <= W <= 10000


(define [memorize-func func]
  (let ([hash-table (make-hash)])
    (λ [goods-lst max-weight]
       (let ([key (cons (length goods-lst) max-weight)])
         (if [hash-has-key? hash-table key] (hash-ref hash-table key)
           (let ([result (func goods-lst max-weight)])
             (hash-set! hash-table key result)
             result))))))


(define [get-max-value-combination goods-lst max-weight]
  (let ([weight-lst (map car goods-lst)]
        [value-lst (map cdr goods-lst)])
    (cond ([null? goods-lst] 0)
          ([< max-weight (apply min weight-lst)] 0)
          ([< max-weight (car weight-lst)] (get-max-value-combination (cdr goods-lst) max-weight))
          (else (max (get-max-value-combination (cdr goods-lst) max-weight)
                     (+ (car value-lst)
                        (get-max-value-combination (cdr goods-lst)
                                                   (- max-weight (car weight-lst)))))))))


((memorize-func get-max-value-combination) (list (cons 2 3) (cons 1 2) (cons 3 4) (cons 2 2)) 5)
