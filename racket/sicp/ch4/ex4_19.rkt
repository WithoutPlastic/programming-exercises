#lang racket

(let ([a 1])
  (define [f x]
    (define b (+ a x))
    (define a 5)
    (+ a b))
  (f 10))

;Answer: The EVA solution is complex, we need create extra layer and draw
;dependency tree, and rearrange the definitions from leaf to root. But the EVA
;solution is my recommendation, especially in a functional programming
;language. And Alyssa is also fine, it is simple, simultaneous, and won't give
;undetermined result, raise error if met this case. And Ben's solution is
;indeed a solution. But for better programming, non-strait-forward design is
;not recommended.
