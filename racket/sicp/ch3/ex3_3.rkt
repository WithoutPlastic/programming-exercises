#lang racket

(define [make-account available-credit password]
  (lambda [op input-password]
    (cond ([not (eq? input-password password)]
           (lambda args "Incorrect password!"))
          ([eq? op 'withdraw]
           (lambda [amount]
             (if [< available-credit amount]
               "Insufficient funds"
               (begin
                 (set! available-credit (- available-credit amount))
                 available-credit))))
          ([eq? op 'deposit]
           (lambda [amount]
             (begin
               (set! available-credit (+ available-credit amount))
               available-credit)))
          (else (error "Unknown operation" op)))))

(define acc (make-account 100 'secret-password))

((acc 'withdraw 'secret-password) 40)

((acc 'deposit 'some-other-password) 50)
