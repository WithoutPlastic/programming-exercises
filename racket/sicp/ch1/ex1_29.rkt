#lang racket

(define [cube x] (* x x x))
(define simpson-sum-thousand-iteration 1000)
(define simpson-sum-hundred-iteration 100)

(define [simpson-sum f a b iteration-times]
  (define [step a b divisions] (/ (- b a) divisions))
  (if [= iteration-times 0]
    0
    (* (/ (step a b iteration-times) 3)
      (+
        (f a)
        (* 4 (f (+ a (step a b iteration-times))))
        (f (+ a (* 2 (step a b iteration-times))))
        (* (/ 3 (step a b iteration-times)) (simpson-sum f (+ a (* 2 (step a b iteration-times))) b (- iteration-times 2)))))))

(simpson-sum cube 0 1 simpson-sum-hundred-iteration)
(simpson-sum cube 0 1 simpson-sum-thousand-iteration)
