#lang racket

;Problem:
;Say you have an array for which the ith element is the price of a given stock
;on day i.
;
;If you were only permitted to complete at most one transaction (ie, buy one and
;sell one share of the stock), design an algorithm to find the maximum profit.)

(define [max-profit prices]
  (define [shift-idx result s] (cons (+ s (car result)) (+ s (cdr result))))
  (define [split len]
    (let ([mid-idx (floor (/ len 2))]
          [idx-price-pairs (map cons (range 0 len) prices)])
      (argmax
        (λ [x] (- (list-ref prices (cdr x)) (list-ref prices (car x))))
        (list (cons (car (argmin cdr (take idx-price-pairs (add1 mid-idx))))
                    (car (argmax cdr (drop idx-price-pairs mid-idx))))
              (max-profit (take prices mid-idx))
              (shift-idx (max-profit (drop prices (add1 mid-idx)))
                         (add1 mid-idx))))))

  (let ([len (length prices)])
    (cond ([= len 1] (cons 0 0))
          ([= len 2] (if [< (car prices) (cadr prices)] (cons 0 1) (cons 0 0)))
          (else (split len)))))

(define test-prices '(5 41 2 3 4 3 2 5 6 2 8 7))

(max-profit test-prices)
