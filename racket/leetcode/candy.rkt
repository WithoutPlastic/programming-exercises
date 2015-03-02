#lang racket

;Problem:
;There are N children standing in a line. Each child is assigned a rating
;value.
;
;You are giving candies to these children subjected to the following
;requirements:
;
; - Each child must have at least one candy.
; - Children with a higher rating get more candies than their neighbors.
;
;What is the minimum candies you must give?

(define [candy ratings]
  (define [shift-min->one ints]
    (map (compose add1 (curryr - (apply min ints))) ints))

  (define [accum-delta base Δs]
    (if [null? Δs] '()
      (let ([next-base (+ base (car Δs))])
        (cons next-base (accum-delta next-base (cdr Δs))))))

  (define [generate-min-delta pair]
    (let ([pr (car pair)] [nr (cdr pair)])
      (cond ([= pr nr] 0) ([< pr nr] +1) (else -1))))

  (let* ([neighbor-ratings (map cons (drop-right ratings 1) (cdr ratings))]
         [deltas (map generate-min-delta neighbor-ratings)])
    (foldl + 0 (shift-min->one (cons 0 (accum-delta 0 deltas))))))

(define test-ratings (build-list 10 (thunk* (random 10))))

(displayln test-ratings)
(candy test-ratings)
