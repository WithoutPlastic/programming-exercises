#lang racket

(define [square x] (* x x))

(define [recur-cont-frac fn fd frac-times]
  (define [frac left-times]
    (let ([cur-seq (+ (- frac-times left-times) 1)])
      (if [= left-times 1]
        (/ (fn cur-seq) (fd cur-seq))
        (/ (fn cur-seq) (- (fd cur-seq) (frac (- left-times 1))))
        )))
  (frac frac-times))

(define [tan-cf x frac-times]
  (recur-cont-frac
    (lambda (i) (if [= i 1] x (square x)))
    (lambda (i) (- (* 2 i) 1))
    frac-times))

(tan-cf 1.0 1000)
