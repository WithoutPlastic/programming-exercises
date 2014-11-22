#lang racket

(define op-table (make-hash)) ; '(op, expr-type) -> specific op
(define [get op expr-type]
  (hash-ref op-table (list op expr-type)))
(define [put op expr-type specific-op]
  (hash-set! op-table (list op expr-type) specific-op))

(define [variable? expr] (symbol? expr))
(define [same-variable? expr-a expr-b]
  (and
    (variable? expr-a)
    (variable? expr-b)
    (eq? expr-a expr-b)))
(define [alt-number? expr num] (and (number? expr) (= expr num)))
(define [operator expr] (car expr))
(define [oeprands expr] (cdr expr))

(define [install-sum-expr-package]
  (define [make-sum expr-a expr-b]
    (cond ([alt-number? expr-a 0] expr-b)
          ([alt-number? expr-b 0] expr-a)
          ([and (number? expr-a) (number? expr-b)] (+ expr-a expr-b))
          (else (list '+ expr-a expr-b))))
  (define [addend expr] (cadr expr))
  (define [augend expr] (caddr expr))
  
  (put 'make-expr '+
       (lambda [operator-left operator-right]
         (make-sum operator-left operator-right)))
  (put 'deriv '+
       (lambda [expr var]
         (make-sum
           (deriv (addend expr) var)
           (deriv (augend expr) var)))))

(define [install-product-expr-package]
  (define [make-product expr-a expr-b]
    (cond ([or (alt-number? expr-a 0) (alt-number? expr-b 0)] 0)
          ([alt-number? expr-a 1] expr-b)
          ([alt-number? expr-b 1] expr-a)
          ([and (number? expr-a) (number? expr-b)] (* expr-a expr-b))
          (else (list '* expr-a expr-b))))
  (define [multiplier expr] (cadr expr))
  (define [multiplcand expr] (caddr expr))

  (put 'make-expr '*
       (lambda [operator-left operator-right]
         (make-product operator-left operator-right)))
  (put 'deriv '*
       (lambda [expr var]
         ((get 'make-expr '+)
          (make-product
            (multiplier expr)
            (deriv (multiplcand expr) var))
          (make-product
            (multiplcand expr)
            (deriv (multiplier expr) var))))))

(define [install-exponentiation-expr-package]
  (define [make-exponentiation expr-a expr-b]
    (cond ([alt-number? expr-b 0] 1)
          ([alt-number? expr-b 1] expr-a)
          ([alt-number? expr-a 0] 0)
          (else
            (list '** expr-a expr-b))))
  (define [base expr] (cadr expr))
  (define [exponent expr] (caddr expr))

  (put 'make-expr '**
       (lambda (operator-left operator-right)
         (make-exponentiation operator-left operator-right)))
  (put 'deriv '**
       (lambda (expr var)
         ((get 'make-expr '*)
           (exponent expr)
           ((get 'make-expr '*)
             (make-exponentiation
               (base expr)
               (- (exponent expr) 1))
             (deriv
               (base expr)
               var))))))

(define [deriv expr var]
  (cond ([number? expr] 0)
        ([and (variable? expr) (not (same-variable? expr var))] 0)
        ([and (variable? expr) (same-variable? expr var)] 1)
        (else 
          ((get 'deriv (operator expr)) expr var))))

(install-sum-expr-package)
(install-product-expr-package)
(install-exponentiation-expr-package)
(deriv '(+ x 3) 'x)
(deriv '(* x y) 'x)
(deriv '(* (* x y) (+ x 3)) 'x)

