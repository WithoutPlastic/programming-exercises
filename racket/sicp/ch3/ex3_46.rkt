#lang racket

;(define [test-and-set! cell]
;  (without-interrupts
;    (lambda []
;      (if [car cell]
;        #t
;        (begin
;          (set-car! cell #t)
;          #f)))))
(define [make-mutex]
  (let ([cell (list #f)])
    (define [test-and-set!]
      (if [car cell]
        #t
        (begin
          (set-car! cell #t)
          #f)))
    (define [clear!]
      (set-car! cell #f))

    (define [the-mutex msg]
      (cond ([eq? msg 'acquire]
             (when [test-and-set!]
               (the-mutex 'acquire)))
            ([eq? msg 'release]
             (clear!))))
    the-mutex))
(define [make-serializer]
  (let ([mutex (make-mutex)])
    (lambda [p]
      (define [serialized-p . args]
        (mutex 'acquire)
        (let ([value (apply p args)])
          (mutex 'release)
          value))
      serialized-p)))
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
  (let* ([protected (make-serializer)]
         [protected-withdraw (protected withdraw)]
         [protected-deposit (protected deposit)])
    (define [dispatch op . args]
      (cond ([eq? op 'withdraw] (apply protected-withdraw args))
            ([eq? op 'deposit] (apply protected-deposit args))
            ([eq? op 'balance] balance)
            (else (error "unknown operation -- MAKE-ACCOUNT" op))))
    dispatch))

(define [exchange account-a account-b]
  (let ([difference (- (account-a 'balance) (account-b 'balance))])
    (account-a 'withdraw difference)
    (account-b 'deposit difference)))

(define [make-account-and-serializer balance]
  (define [withdraw amount]
    (if [<= amount balance]
      (begin
        (set! balance (- balance amount))
        balance)
      "Insufficient funds"))
  (define [deposit amount]
    (set! balance (+ balance amount))
    balance)
  (let ([balance-serializer (make-serializer)])
    (define [dispatch op]
      (cond ([eq? op 'withdraw] (balance-serializer withdraw))
            ([eq? op 'deposit] (balance-serializer deposit))
            ([eq? op 'balance] balance)
            ([eq? op 'serializer] balance-serializer)
            (else (error "unknown operation -- MAKE-ACCOUNT" op))))
    dispatch))

(define [deposit account amount] ((account 'deposit) amount))
(define [withdraw account amount] ((account 'withdraw) amount))
(define [serialized-exchange account-a account-b]
  (let ([serializer-a (account-a 'serializer)]
        [serializer-b (account-b 'serializer)])
    ((serializer-a (serializer-b exchange)) account-a account-b)))

(define [transfer from-account to-account amount]
  ((from-account 'withdraw) amount)
  ((to-account 'deposit) amount))

;Answer: test-and-set! is not atomic operation, one condition step, one set
;step
