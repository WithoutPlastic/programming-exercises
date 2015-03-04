#lang racket

;Problem:
;Say you have an array for which the ith element is the price of a given stock
;on day i.
;
;Design an algorithm to find the maximum profit. You may complete at most two
;transactions.
;
;Note:
;You may not engage in multiple transactions at the same time (ie, you must sell
;the stock before you buy again).

(define [combine-pos/negs ints]
  (if [= (length ints) 1] ints
    (let ([first-n (car ints)] [second-n (cadr ints)] [rests (cdr ints)])
      (if [xor [positive? first-n] [positive? second-n]]
        (cons first-n (combine-pos/negs rests))
        (combine-pos/negs (cons (+ first-n second-n) (cdr rests)))))))

(define [merge-big-increases pos/neg-ints]
  (if [< (length pos/neg-ints) 3] pos/neg-ints
    (let ([first-n (car pos/neg-ints)] [second-n (cadr pos/neg-ints)]
          [third-n (caddr pos/neg-ints)] [rests (cdr pos/neg-ints)])
      (cond ([< first-n 0] (cons first-n (merge-big-increases rests)))
            ([< (min first-n third-n) (- second-n)]
             (cons first-n (cons second-n (merge-big-increases (cdr rests)))))
            (else (let ([merged-elt (+ first-n second-n third-n)])
                    (merge-big-increases (cons merged-elt (cddr rests)))))))))

(define [small-decreases pos/neg-ints]
  (if [< (length pos/neg-ints) 3] '()
    (let ([first-n (car pos/neg-ints)] [second-n (cadr pos/neg-ints)]
          [thid-n (caddr pos/neg-ints)] [rests (cdr pos/neg-ints)])
      (cond ([< first-n 0] (small-decreases rests))
            ([< (min first-n thid-n) (- second-n)]
             (small-decreases (cdr rests)))
            (else (cons second-n (small-decreases (cdr rests))))))))

(define [max-profit-with-n-transactions n prices]
  (let* ([Δps (map - (cdr prices) (drop-right prices 1))]
         [combined-Δps (combine-pos/negs Δps)]
         [small-neg-Δps (small-decreases combined-Δps)]
         [pos-merged-Δps (filter positive? (merge-big-increases combined-Δps))]
         [max-transaction (+ (length pos-merged-Δps) (length small-neg-Δps))])
    (foldl + 0 (take (sort (map abs (append pos-merged-Δps small-neg-Δps)) >)
                     (if [< n max-transaction] n max-transaction)))))
(provide max-profit-with-n-transactions)

(define max-profit (curry max-profit-with-n-transactions 2))

(define test-prices '(5 41 2 3 4 3 2 5 6 2 8 7))

;(max-profit test-prices)
