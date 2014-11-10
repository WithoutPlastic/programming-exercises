#lang racket

(define (recursive-f n)
  (if (< n 3)
    n
    (+
      (recursive-f (- n 1))
      (* 2 (recursive-f (- n 2)))
      (* 3 (recursive-f (- n 3))))))

(define (iter-compute minor-one-result minor-two-result minor-three-result)
  (+ minor-one-result (* 2 minor-two-result) (* 3 minor-three-result)))

(define (linear-f-iter 
          count 
          minor-one-result 
          minor-two-result 
          minor-three-result)
  (if (= count 3)
    (iter-compute minor-one-result minor-two-result minor-three-result)
    (linear-f-iter
      (- count 1)
      (iter-compute minor-one-result minor-two-result minor-three-result)
      minor-one-result
      minor-two-result
      )))

(define (linear-f n)
  (cond ((< n 3) n)
        (else (linear-f-iter
                n
                (linear-f 2)
                (linear-f 1)
                (linear-f 0)
                ))))

(recursive-f 0)
(linear-f 0)
(recursive-f 1)
(linear-f 1)
(recursive-f 2)
(linear-f 2)
(recursive-f 3)
(linear-f 3)
(recursive-f 4)
(linear-f 4)
(recursive-f 12)
(linear-f 12)
