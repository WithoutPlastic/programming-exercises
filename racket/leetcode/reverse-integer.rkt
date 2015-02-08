#lang racket

;Problem:
;Reverse digits of an integer.

(define [reverse-integer int]
  (define [iter remaining-digits accum-digits]
    (let* ([cur-digits (remainder remaining-digits 10)]
           [next-remaining-digits (floor (/ remaining-digits 10))]
           [next-accum-digits (+ cur-digits (* 10 accum-digits))])
      (if [= 0 next-remaining-digits]
        next-accum-digits
        (iter next-remaining-digits next-accum-digits))))
  (iter int 0))

(reverse-integer 1024)
