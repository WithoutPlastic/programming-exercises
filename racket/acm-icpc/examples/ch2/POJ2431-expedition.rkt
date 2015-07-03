#lang racket

;Problem:
;You need to drive a track to go L unit distance. In the beginning, there is P unit oil. Track
;comsume one unit old per unit distance. If oil run out, track can't go further. Then you can't
;reach destination. But between start point and destination, there is N oil station, i-th station
;located at Ai from start point with Bi unit oil. Assuming the oil tank is large enough, infinity.
;
;Write program to answer question:
;- Can you reach the destination?
;- If yes, need to how many times oil supply as least?
;
;Restriction:
;1 <= N <= 10000
;1 <= L <= 1000000, 1 <= P <= 1000000
;1 <= Ai < L, 1 <= Bi <= 100


(require data/heap)


(define [find-min-oil-supply station-lst dest-distance init-oil]
  (let iter ([current-location 0]
             [remaining-station-lst (sort station-lst (λ [a b] [< (car a) (car b)]))]
             [unused-station-lst (make-heap (λ [a b] [< (cdr b) (cdr a)]))]
             [remaining-oil init-oil])
    (let* ([next-distance (+ current-location remaining-oil)]
           [station-passed? (λ [elt] [<= (car elt) next-distance])]
           [passed-station-lst (takef remaining-station-lst station-passed?)]
           [non-passed-station-lst (dropf remaining-station-lst station-passed?)])
      (heap-add-all! unused-station-lst passed-station-lst)
      (cond ([<= dest-distance next-distance]
             (- (length station-lst) (heap-count unused-station-lst)))
            ([= (heap-count unused-station-lst) 0] false)
            (else (let ([supply-station (heap-min unused-station-lst)])
                    (heap-remove-min! unused-station-lst)
                    (iter next-distance
                          non-passed-station-lst
                          unused-station-lst
                          (cdr supply-station))))))))


(find-min-oil-supply (list (cons 10 10) (cons 14 5) (cons 20 2) (cons 21 4)) 25 10)
