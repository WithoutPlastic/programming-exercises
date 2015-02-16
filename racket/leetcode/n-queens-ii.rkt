#lang racket

;Problem:
;Follow up for N-Queens problem.
;
;Now, instead outputting board configurations, return the total number of
;distinct solutions.

(require "n-queens.rkt")

(define total-n-queens (compose length solve-n-queens))

(total-n-queens 4)
(total-n-queens 5)
(total-n-queens 6)
(total-n-queens 7)
(total-n-queens 8)
