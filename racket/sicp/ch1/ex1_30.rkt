#lang racket

(define [cube x] (* x x x))
(define simpson-sum-thousand-iteration 1000)
(define simpson-sum-hundred-iteration 100)

(define [tail-calculator f a step next-result]
  (* (/ step 3)
    (+
      (f a)
      (* 4 (f (+ a step)))
      (f (+ a (* 2 step)))
      (* (/ 3 step) next-result))))

(define [simpson-sum f a b iteration-times]
  (define step (/ (- b a) iteration-times))
  (define (iter a)
    (if [< a b]
      (tail-calculator f a step (iter (+ a (* 2 step))))
      0))
  (iter a))

(simpson-sum cube 0 1 simpson-sum-hundred-iteration)
(simpson-sum cube 0 1 simpson-sum-thousand-iteration)
