#lang racket

(define [an-integer-between low-boundary high-boundary]
  (require (<= low-boundary high-boundary))
  (amb a (an-integer-between (+ a 1) b)))

(define [safe? solution]
  (let ([p (car solution)])
    (define [conflict? q i]
      [or [= p q]
          [= p (+ q i)]
          [= p (- q i)]])
    (define [check rest i]
      (cond ([null? rest] true)
            ([conflict? (car rest) i] false)
            (else (check (cdr rest) (add1 i)))))
    (check (cdr solution) 1)))

(define [queens n]
  (define [iter solution n-left]
    (cond ([= n-left 0]
           (display solution) (newline))
          (else
            (let ([x-solution (cons (an-integer-between 1 n) solution)])
              (require [safe? x-solution])
              (iter x-solution (- n-left 1))))))
  (iter '() n))

(queens 8)
