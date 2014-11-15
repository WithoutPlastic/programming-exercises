#lang racket

(define [repeat-f f times]
  (define [repeat times]
    (if [= times 1]
      f
      (lambda (x) (f ((repeat (- times 1)) x)))))
  (repeat times))

(define [smooth f]
  (let ([smooth-step 0.01])
    (lambda (x)
      (/ (+
           (f (- x smooth-step))
           (f x)
           (f (+ x smooth-step)))
         3))))

(define [repeat-smooth f times]
  ((repeat-f smooth times) f))
