#lang racket

;Problem:
;Say you have an array for which the ith element is the price of a given stock
;on day i.
;
;Design an algorithm to find the maximum profit. You may complete at most k
;transactions.
;
;Note:
;You may not engage in multiple transactions at the same time (ie, you must sell
;the stock before you buy again).
;
;Credits:
;Special thanks to @Freezen for adding this problem and creating all test cases.

(require "best-time-to-buy-and-sell-stock-iii.rkt")

(define max-profit max-profit-with-n-transactions)

(define test-prices '(5 41 2 3 4 3 2 5 6 2 8 7))

(max-profit 4 test-prices)
