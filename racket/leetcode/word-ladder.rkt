#lang racket

;Problem:
;Given two words (start and end), and a dictionary, find the length of shortest
;transformation sequence from start to end, such that:
;
; - Only one letter can be changed at a time
; - Each intermediate word must exist in the dictionary
;
;For example,
;
;Given:
;start = "hit"
;end = "cog"
;dict = ["hot","dot","dog","lot","log"]
;
;As one shortest transformation is "hit" -> "hot" -> "dot" -> "dog" -> "cog",
;return its length 5.
;
;Note:
;
; - Return 0 if there is no such transformation sequence.
; - All words have the same length.
; - All words contain only lowercase alphabetic characters.

(define [ladder-length start end dictionary]
  (define [iter f-chars r-dict]
    (let ([distance (λ [x] (length (filter-map (negate eq?) f-chars x)))])
      (cond ([= (distance (string->list end)) 1] 1)
            ([null? r-dict] +inf.0)
            (else (add1
                    (apply min
                      (map (λ [n] (iter n (filter-not (curry equal? n) r-dict)))
                           (filter (compose (curry = 1) distance) r-dict))))))))

  (let ([trans-times (iter (string->list start) (map string->list dictionary))])
    (if [eq? trans-times +inf.0] 0
      (add1 trans-times))))

(define test-start "hit")
(define test-end "cog")
(define test-dictionary '("hot" "dot" "dog" "lot" "log"))

(ladder-length test-start test-end test-dictionary)
