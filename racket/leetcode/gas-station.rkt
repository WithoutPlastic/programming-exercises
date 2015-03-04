#lang racket

;Problem:
;There are N gas stations along a circular route, where the amount of gas at
;station i is gas[i].
;
;You have a car with an unlimited gas tank and it costs cost[i] of gas to
;travel from station i to its next station (i+1). You begin the journey with an
;empty tank at one of the gas stations.
;
;Return the starting gas station's index if you can travel around the circuit
;once, otherwise return -1.
;
;Note: The solution is guaranteed to be unique.

(define [shift-left lst offset] (append (drop lst offset) (take lst offset)))

(define [get-circuit-complete-idxs gass roads]
  (let* ([path (append-map list gass (map - roads))]
         [idxs (range 0 (length gass))]
         [complete? (λ [Δ r] [and r [< 0 r] [< 0 (+ r Δ)] (+ r Δ)])]
         [forward-ok? (λ [p] (foldl complete? (car p) (cdr p)))]
         [backward-ok? (λ [p] (foldr complete? (car p) (cdr p)))]
         [-1-when-empty (λ [x] (if [null? x] -1 x))])
    (-1-when-empty
      (filter-map (λ [idx p] [and [or [forward-ok? p] [backward-ok? p]] idx])
                  idxs (map (curry shift-left path) (map (curry * 2) idxs))))))

(define gas '(8 1 3 6 10 4))
(define road '(3 1 7 10 4 6))

(get-circuit-complete-idxs gas road)
