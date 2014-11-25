#lang racket

(define [random-in-range low high] (+ low (random (- high low))))
(define [monte-carlo trials experiment]
  (define [iter left-trials passed-trials]
    (cond ([= left-trials 0] (/ passed-trials trials))
          ((experiment)
           (iter (sub1 left-trials) (add1 passed-trials)))
          (else
            (iter (sub1 left-trials) passed-trials))))
  (iter trials 0))

(define [estimate-integral]
  (let* ([circle-mid-x 5]
         [circle-mid-y 7]
         [circle-range 100] ;-> Accuration of Pi
         [rectangular-high-y (+ circle-mid-y circle-range)]
         [rectangular-low-y (- circle-mid-y circle-range)]
         [rectangular-high-x (+ circle-mid-x circle-range)]
         [rectangular-low-x (- circle-mid-x circle-range)]
         [in-circle? (lambda [pos-x pos-y] 
                       (<= (+ (expt (- pos-x circle-mid-x) 2)
                             (expt (- pos-y circle-mid-y) 2))
                          (expt circle-range 2)))])
    (in-circle? (random-in-range rectangular-low-x rectangular-high-x)
                (random-in-range rectangular-low-y rectangular-high-y))))

(define [estimate-pi]
  (* 4.0 (monte-carlo 1000000 estimate-integral)))

(estimate-pi)
