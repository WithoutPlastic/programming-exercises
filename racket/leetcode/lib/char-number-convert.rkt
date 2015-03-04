#lang racket

;Convert
(define [char->number c] (- (char->integer c) 48))
(define [number->char n] (integer->char (+ n 48)))

(define [chars->number chars]
  (let* ([len (length chars)]
         [dec-weights (reverse (map (curry expt 10) (range 0 len)))])
    (apply + (map * (map char->number chars) dec-weights))))

(define [get-width int]
  (define [iter e] (if [= 0 (floor (/ int (expt 10 e)))] e (iter (add1 e))))
  (iter 1))

(define [number->chars int]
  (let* ([width (get-width int)]
         [dec-weights (reverse (map (curry expt 10) (range 0 width)))])
    (map (λ [w] (number->char (remainder (floor (/ int w)) 10))) dec-weights)))

(provide (all-defined-out))
