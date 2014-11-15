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

(define [f-oula idx]
  (cond ([or (= (remainder idx 3) 0) (= (remainder idx 3) 1)] 1)
        (else (/ (* 2 (+ idx 1)) 3))))

(+ 2 (recur-cont-frac (lambda (i) 1.0) f-oula 1000))
