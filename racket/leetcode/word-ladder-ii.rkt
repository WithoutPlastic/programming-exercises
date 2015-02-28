#lang racket

;Problem:
;Given two words (start and end), and a dictionary, find all shortest
;transformation sequence(s) from start to end, such that:
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
;Return
;
;[
;  ["hit","hot","dot","dog","cog"],
;  ["hit","hot","lot","log","cog"]
;]
;
;Note:
;
; - All words have the same length.
; - All words contain only lowercase alphabetic characters.

(define [find-ladder start end dictionary]
  (define [filter-shortest-lists lol]
    (if [null? lol] '()
      (let ([min-len (apply min (map length lol))])
        (filter (λ [l] [= (length l) min-len]) lol))))

  (define [iter f-chars r-dict]
    (let ([first-str (list->string f-chars)]
          [distance (λ [x] (length (filter-map (negate eq?) f-chars x)))])
      (cond ([= (distance (string->list end)) 1]
             (list (list first-str end)))
            ([null? r-dict] '())
            (else (append-map
                    (λ [n] (map (curry cons first-str)
                                (iter n (filter-not (curry equal? n) r-dict))))
                    (filter (compose (curry = 1) distance) r-dict))))))

  (filter-shortest-lists
    (iter (string->list start) (map string->list dictionary))))

(define test-start "hit")
(define test-end "cog")
(define test-dictionary '("hot" "dot" "dog" "lot" "log"))

(find-ladder test-start test-end test-dictionary)
