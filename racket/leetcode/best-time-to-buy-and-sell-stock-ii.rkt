#lang racket

;Problem:
;Say you have an array for which the ith element is the price of a given stock
;on day i.
;
;Design an algorithm to find the maximum profit. You may complete as many
;transactions as you like (ie, buy one and sell one share of the stock multiple
;times).
;
;However, you may not engage in multiple transactions at the same time (ie, you
;must sell the stock before you buy again).

(define [max-profit prices]
  (define [count-in-ascendings idx]
    (let ([next-idx (add1 idx)])
      (if [<= (length prices) next-idx] 0
        (let ([Δprice (- (list-ref prices next-idx) (list-ref prices idx))])
          (+ (if [< 0 Δprice] Δprice 0) (count-in-ascendings next-idx))))))

  (count-in-ascendings 0))

(define test-prices '(5 41 2 3 4 3 2 5 6 2 8 7))

(max-profit test-prices)
