#lang racket
(define (sum-bigger-two-of-three a b c)
  (cond ((and (< a b) (< a c)) (+ b c))
        ((and (< b a) (< b c)) (+ a c))
        ((and (< c a) (< c b)) (+ a b))))

(define (max x y) (if (< x y) y x))
(define (min x y) (if (< x y) x y))
(define (advanced-sum-bigger-two-of-three a b c)
  (+ (max a b) (max (min a b) c)))

(sum-bigger-two-of-three 5 6 7)
(advanced-sum-bigger-two-of-three 5 6 7)
