#lang racket

;Problem:
;There is n element, no difference. Divide them to m non-empty group.
;
;Calculate remaining number of the divide solution's count mod M.
;
;Restriction:
;1 <= m <= n <= 1000
;2 <= M <= 10000


(require "../../lib/memorize-function.rkt")


(define [find-stirling-number n m mod]
  (define [iter n m]
    (if [or [= m n] [= m 1]] 1
      (+ (iter (sub1 n) m) (iter n (sub1 m)))))

  (unless [and [< n m] [< 0 n] [< 0 m]]
    (remainder ((memorize-func iter) n m) mod)))


(find-stirling-number 4 3 10000)
