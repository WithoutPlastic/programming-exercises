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
            ([eq? op 'balance] (balance))
            (else (error "unknown operation -- MAKE-ACCOUNT" op))))
    dispatch))

(define x 10)
(define s (make-serializer))
(parallel-execute (lambda [] (set! x ((s (lambda [] (* x x))))))
                  (s (lambda [] (set! x ()))))

;Parallel execution cases
;
;atomic procedure add1-apply-changes, square, apply-square-changes
;apply-square-changes always after square
;
;Time elapse ----------------------------------------------------------------->
;case 0
;initial        add1-apply-changes      square          apply-square-changes
; 10                11                  11(121)                 121
;
;case 1
;initial        square          apply-square-changes    add1-apply-changes
; 10                10(121)             100                     101
;
;case 2
;initial        square          add1-apply-changes      apply-square-changes
; 10                10(100)             11(100)                 100
