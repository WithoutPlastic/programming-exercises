#lang racket

(define [make-serializer f] (lambda [] (f)))
(define [parallel-execute . fs]
  (if [null? fs]
    'done
    (begin
      (car fs)
      (parallel-execute (cdr fs)))))

(define [make-account balance]
  (define [withdraw amount]
    (if [<= amount balance]
      (begin
        (set! balance (- balance amount))
        balance)
      "Insufficient funds"))
  (define [deposit amount]
    (set! balance (+ balance amount))
    balance)
  (let ([protected (make-serializer)])
    (define [dispatch op . args]
      (cond ([eq? op 'withdraw] (apply (protected withdraw) args))
            ([eq? op 'deposit] (apply (protected deposit) args))
            ([eq? op 'balance] (protected (lambda [] balance)))
            (else (error "unknown operation -- MAKE-ACCOUNT" op))))
    dispatch))

;Answer: It depends, if accessing a variable is atomic, it is no need.
