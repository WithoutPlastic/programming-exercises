#lang racket

(define [variable? expr] (symbol? expr))
(define [same-variable? expr-a expr-b]
  (and
    (variable? expr-a)
    (variable? expr-b)
    (eq? expr-a expr-b)))

(define [alt-number? expr num] (and (number? expr) (= expr num)))
(define [sum? expr] (and (pair? expr) (eq? (car expr) '+)))
;(define [make-sum expr-a expr-b] (list '+ expr-a expr-b))
(define [make-sum expr-a expr-b]
  (cond ([alt-number? expr-a 0] expr-b)
        ([alt-number? expr-b 0] expr-a)
        ([and (number? expr-a) (number? expr-b)] (+ expr-a expr-b))
        (else (list '+ expr-a expr-b))))
(define [addend expr] (cadr expr))
(define [augend expr]
  (if [and (not (null? (cddr expr))) (null? (cdddr expr))]
    (caddr expr)
    (make-sum
      (cadr expr)
      (augend
        (make-sum (caddr expr) (cadddr expr))))))

(define [product? expr] (and (pair? expr) (eq? (car expr) '*)))
;(define [make-product expr-a expr-b] (list '* expr-a expr-b))
(define [make-product expr-a expr-b]
  (cond ([or (alt-number? expr-a 0) (alt-number? expr-b 0)] 0)
        ([alt-number? expr-a 1] expr-b)
        ([alt-number? expr-b 1] expr-a)
        ([and (number? expr-a) (number? expr-b)] (* expr-a expr-b))
        (else (list '* expr-a expr-b))))
(define [multiplier expr] (cadr expr))
(define [multiplcand expr]
  (if [and (not (null? (cddr expr))) (null? (cdddr expr))]
    (caddr expr)
    (make-product
      (cadr expr)
      (multiplcand
        (make-product (caddr expr) (cadddr expr))))))

(define [exponentiation? expr] (and (pair? expr) (eq? (car expr) '**)))
(define [make-exponentiation expr-a expr-b]
  (cond ([alt-number? expr-b 0] 1)
        ([alt-number? expr-b 1] expr-a)
        ([alt-number? expr-a 0] 0)
        (else
          (list '** expr-a expr-b))))
(define [base expr] (cadr expr))
(define [exponent expr] (caddr expr))

(define [deriv expr var]
  (cond ([number? expr] 0)
        ([and (variable? expr) (not (same-variable? expr var))] 0)
        ([and (variable? expr) (same-variable? expr var)] 1)
        ([sum? expr]
         (make-sum
           (deriv (addend expr) var)
           (deriv (augend expr) var)))
        ([product? expr]
         (make-sum
           (make-product
             (multiplier expr)
             (deriv (multiplcand expr) var))
           (make-product
             (multiplcand expr)
             (deriv (multiplier expr) var))))
        ([exponentiation? expr]
         (make-product
           (exponent expr)
           (make-product
             (make-exponentiation
               (base expr)
               (- (exponent expr) 1))
             (deriv
               (base expr)
               var))))
        (else
          (error "Unknown expression type -- DERIV" expr))))

(deriv '(+ x 3) 'x)
(deriv '(* x y) 'x)
(deriv '(* (* x y) (+ x 3)) 'x)
(deriv '(* x y (+ x 3)) 'x)
