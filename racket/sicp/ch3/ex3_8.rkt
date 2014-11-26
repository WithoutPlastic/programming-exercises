#lang racket

(define f
  (let ([called-times 0])
    (lambda [x]
      (if [= (add1 called-times )1]
        (begin
          (set! called-times (add1 called-times))
          x)
        0))))

(+ (f 1) (f 0))

;(+ (f 0) (f 1))
