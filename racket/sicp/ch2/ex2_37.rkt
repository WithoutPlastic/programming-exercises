#lang racket

(define [accumulate op initial sequence]
  (if [null? sequence]
    initial
    (op
      (car sequence)
      (accumulate op initial (cdr sequence)))))

(define [accumulate-n op init seqs]
  (if [null? (car seqs)]
    '()
    (cons (accumulate op init (map car seqs))
          (accumulate-n op init (map cdr seqs)))))

(define [dot-product v w]
  (accumulate + 0 (map * v w)))

(define [matrix-*-vector m v]
  (map (lambda (r) (dot-product v r)) m))

(define [transpose mat]
  (accumulate-n
    (lamdba (x y) (append (list x) y))
    '()
    mat))

(define [matrix-*-matrix m n]
  (let ([cols (transpose n)])
    (map
      (lambda (r)
        (matrix-*-vector n r))
      m)))
(accumulate-n + 0 (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))
