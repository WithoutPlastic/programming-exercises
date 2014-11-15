#lang racket

(define [average a b] (/ (+ a b) 2))
(define fixpoint-torlerance 0.00001)
(define [close-enough? a b how-close] (< (abs (- a b)) how-close))

(define [fixed-point f init-guess]
  (define [iter cur-guess]
    (let ([next-guess (f cur-guess)])
      (if [close-enough? cur-guess next-guess fixpoint-torlerance]
        (/ (+ cur-guess next-guess) 2)
        (iter next-guess)
          )))
  (iter init-guess))

(define [average-damp f]
  (lambda (x) (average x (f x))))

(define [repeat f times]
  (define [iter times]
    (if [= times 1]
      f
      (lambda (x) (f ((iter (- times 1)) x)))))
  (iter times))

(define [n-level-root-via-avg-times number n avg-times]
  (fixed-point
    ((repeat average-damp avg-times)
     (lambda (y)
       (/ number (expt y (- n 1))))) 1.0))

(n-level-root-via-avg-times (expt 2 2) 2 1)
(n-level-root-via-avg-times (expt 2 3) 3 1)
;(n-level-root-via-avg-times (expt 2 4) 4 1)

(n-level-root-via-avg-times (expt 2 4) 4 2)
(n-level-root-via-avg-times (expt 2 5) 5 2)
(n-level-root-via-avg-times (expt 2 6) 6 2)
(n-level-root-via-avg-times (expt 2 7) 7 2)
;(n-level-root-via-avg-times (expt 2 8) 8 2)

(n-level-root-via-avg-times (expt 2 8) 8 3)
(n-level-root-via-avg-times (expt 2 9) 9 3)
(n-level-root-via-avg-times (expt 2 10) 10 3)
(n-level-root-via-avg-times (expt 2 11) 11 3)
(n-level-root-via-avg-times (expt 2 12) 12 3)
(n-level-root-via-avg-times (expt 2 13) 13 3)
(n-level-root-via-avg-times (expt 2 14) 14 3)
(n-level-root-via-avg-times (expt 2 15) 15 3)
;(n-level-root-via-avg-times (expt 2 16) 16 3)

(n-level-root-via-avg-times (expt 2 16) 16 4)
(n-level-root-via-avg-times (expt 2 17) 17 4)
(n-level-root-via-avg-times (expt 2 18) 18 4)
(n-level-root-via-avg-times (expt 2 19) 19 4)
(n-level-root-via-avg-times (expt 2 20) 20 4)
