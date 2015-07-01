#lang racket

;Problem(POJ 3069):
;There is N point on line. The position represented as Xi. Choose and mark some of them. For those
;marked point, it should exist another marked point in R distance(treat self marked point as
;distance zero). Under this requirement, how many point should be marked at least?

;Restriction:
;0 <= N <= 1000
;0 <= R <= 1000
;0 <= Xi <= 1000


(define [find-idx-of-largest-num-below lst target]
  (if [or [null? lst] [< target (car lst)]] false
    (let ([len (length lst)])
      (let iter ([idx 0])
        (cond ([<= len idx] (sub1 len))
              ([< target (list-ref lst idx)] (sub1 idx))
              (else (iter (add1 idx))))))))


(define [find-min-needed-point point-lst range]
  (let iter ([counter 0] [point-lst point-lst])
    (displayln point-lst)
    (if [null? point-lst] counter
      (let* ([stub-point (car point-lst)]
             [right-most-pos (+ stub-point range)]
             [mark-idx (find-idx-of-largest-num-below point-lst right-most-pos)]
             [mark-point (list-ref point-lst mark-idx)])
        (iter (add1 counter)
              (filter-not (curryr <= (+ mark-point range)) point-lst))))))


(find-min-needed-point '(1 7 15 20 30 50) 10)
