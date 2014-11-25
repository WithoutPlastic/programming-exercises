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

(define [make-joint account password new-password]
  (define incorrect-password-return-msg "Incorrect password!" )
  (if [equal? ((account 'deposit password) 0.01) incorrect-password-return-msg]
    (lambda [op input-password] (lambda args incorrect-password-return-msg))
    (lambda [op input-password]
      (if [eq? input-password new-password]
        (account op password)
        (lambda args incorrect-password-return-msg)))))

(define peter-acc (make-account 100 'open-sesame))
(define paul-acc (make-joint peter-acc 'open-sesame 'rosebud))

((peter-acc 'deposit 'open-sesame) 50)
((paul-acc 'deposit 'rosebud) 50)

(define new-paul-acc (make-joint peter-acc 'open 'rosebud))
((new-paul-acc 'deposit 'rosebud) 10)
((new-paul-acc 'withdraw 'rosebud) 10)
