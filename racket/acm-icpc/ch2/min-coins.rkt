#lang racket

;Problem:
;There is 1, 5 10, 50, 100, 500 valued coins, with amount A1, A2, ... A6. Now need to pay for total
;A. Wrte a solution to calculate min amount of coins needed.
;
;Restriction:
;0 <= A1, A2, ... A6 <= 10^9
;0 <= A <= 10^9


(define [calculate-min-coins coin-lst total]
  (let ([value-descending-coin-lst (sort coin-lst (λ [a b] [< (car b) (car a)]))])
    (let iter ([amount 0] [coin-lst value-descending-coin-lst] [remaining total])
      (cond ([< remaining 0] 'not-solved)
            ([= remaining 0] amount)
            (else (let* ([cur-coin-value (caar coin-lst)]
                         [cur-coin-amount (cdar coin-lst)]
                         [pick-amount (min (floor (/ remaining cur-coin-value)) cur-coin-amount)])
                    (iter (+ amount pick-amount)
                          (cdr coin-lst)
                          (- remaining (* pick-amount cur-coin-value)))))))))

(calculate-min-coins
  (list (cons 1 3) (cons 5 2) (cons 10 1) (cons 50 3) (cons 100 0) (cons 500 2))
  620)
