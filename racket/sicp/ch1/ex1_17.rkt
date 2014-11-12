#lang racket

(define (linear-multiple a b)
  (if (= b 0)
    0
    (+ a (linear-multiple a (- b 1)))))

(define (double x) (* x 2))
(define (halve x) (/ x 2))

(define (recursive-fast-multiple a b)
  (cond 
    ((= b 0) 0)
    ((even? b) (recursive-fast-multiple (double a) (halve b)))
    (else (+ a (recursive-fast-multiple (double a) (- b 1))))))

(recursive-fast-multiple 16 16)
(recursive-fast-multiple 0 16)
(recursive-fast-multiple 16 0)
        
(define (linear-iter-fast-multiple a b)
  (define (iter a b product)
    (cond
      ((= b 0) product)
      ((even? b) (iter (double a) (halve b) product))
      (else (iter a (- b 1) (+ product a)))))
  (iter a b 0))

(linear-iter-fast-multiple 16 16)
(linear-iter-fast-multiple 0 16)
(linear-iter-fast-multiple 16 0)
