#lang racket

(define [self-evaluating? expr])

(define [if? expr])
(define [if-predicate expr])
(define [if-consequent expr])
(define [if-alternative expr])
(define [eval-if expr env]
  (if [true? (evlt (if-predicate expr) env)]
    (evlt (if-consequent expr) env)
    (evlt (if-alternative expr) env)))

(define [variable? expr])
(define [quoted? expr])
(define [assignment? expr])
(define [definition? expr])
(define [lambda? expr])
(define [first-expr exprs])
(define [rest-exprs exprs])
(define [last-expr? exprs])
(define [eval-sequence exprs env]
  (if [last-expr? exprs]
    (evlt (first-expr exprs) env)
    (begin
      (evlt (first-expr exprs) env)
      (eval-sequence (rest-exprs exprs) env))))
(define [begin? expr])
(define [cond? expr])

(define [assignment-variable expr])
(define [assignment-value expr])
(define [set-variable-value! variable new-value env])
(define [eval-assignment expr env]
  (set-variable-value! (assignment-variable expr)
                       (evlt (assignment-value expr) env)
                       env)
  'assignment-ok)

(define [definition-variable expr])
(define [definition-value expr])
(define [define-variable! variable init-value env])
(define [eval-definition expr env]
  (define-variable! (definition-variable expr)
                    (evlt (definition-value expr) env)
                    env)
  'definition-ok)

(define [no-operand? operands] [null? operands])
(define [first-operands operands] (car operands))
(define [rest-of-operands operands] (cdr operands))
(define [list-of-values operands env]
  (if [no-operand? operands]
    '()
    (cons (evlt (first-operands operands) env)
          (list-of-values (rest-of-operands operands) env))))
(define [left-right-list-of-values operands env]
  (if [no-operand? operands]
    '()
    (let ([first-evaled-value (evlt (first-operands operands) env)])
      (cons first-evaled-value
            (left-right-list-of-values (rest-of-operands operands) env)))))
(define [right-left-list-of-values operands env]
  (if [no-operand? operands]
    '()
    (let ([rest-evaled-values (right-left-list-of-values (rest-of-operands)
                                                         env)])
      (cons (evlt (first-operands operands) env)
            rest-evaled-values))))

(define [application? expr])

(define [evlt expr env]
  (cond ([self-evaluating? expr] expr)
        ([variable? expr] (lookup-variable-value expr env))
        ([quoted? expr] (text-of-quotation expr))
        ([assignment? expr] (eval-assignment expr env))
        ([definition? expr] (eval-definition expr env))
        ([if? expr] (eval-if expr env))
        ([lambda? expr]
         (make-procedure (lambda-parameters expr)
                         (lambda-body expr)
                         env))
        ([begin? expr] (eval-sequence (begin-actions expr) env))
        ([cond? expr] (evlt (cond->ifs expr) env))
        ([application? expr]
         (aply (evlt (operator expr) env)
               (list-of-values (operands expr) env)))
        (else
          (error "unknown expression type -- EVLT" expr))))

(define [primitive-procedure? procedure])
(define [compound-procedure? procedure])
(define [procedure-environment procedure])
(define [procedure-body procedure])
(define [apply-primitive-procedure procedure arguments])
(define [extend-environment paramters arguments env])
(define [aply procedure arguments]
  (cond ([primitive-procedure? procedure]
         (apply-primitive-procedure procedure arguments))
        ([compound-procedure? procedure]
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameters procedure)
             arguments
             (procedure-environment procedure))))
        (else
          (error "unknown procedure type -- APLY" procedure))))
