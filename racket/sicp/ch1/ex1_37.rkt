#lang racket

(define [recur-cont-frac fn fd frac-times]
  (define [frac left-times]
    (let ([cur-seq (+ (- frac-times left-times) 1)])
      (if [= left-times 1]
        (/ (fn cur-seq) (fd cur-seq))
        (/ (fn cur-seq) (+ (fd cur-seq) (frac (- left-times 1))))
        )))
  (frac frac-times))

(define [iter-cont-frac fn fd frac-times]
  (define [frac left-times result]
    (if [= left-times 0]
      result
      (frac (- left-times 1)
            (/ (fn left-times)
               (+
                 (fd left-times)
                 result)))))
  (frac frac-times 0))

(recur-cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 10)
(recur-cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 11)
(recur-cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 12)
(recur-cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 13)
(recur-cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 14)
(recur-cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 15)
(recur-cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 16)
(recur-cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 17)
(recur-cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 18)
(recur-cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 19)
(recur-cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 20)
(recur-cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 100)
(newline)
(iter-cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 10)
(iter-cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 11)
(iter-cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 12)
(iter-cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 13)
(iter-cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 14)
(iter-cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 15)
(iter-cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 16)
(iter-cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 17)
(iter-cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 18)
(iter-cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 19)
(iter-cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 20)
(iter-cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 100)
