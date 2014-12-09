#lang racket

;with high priority, non-self-evaluating and non-variable expr indicate a
;tagged-expr
(define [self-evaluating? expr]
  [cond ([number? expr] #t)
        ([string? expr] #t)
        (else #f)])
(define [variable? expr] [symbol? expr])

;tagged-expr processing
(define [tagged-expr-tag tagged-expr] (car tagged-expr))
(define [tagged-expr-body tagged-expr] (cdr tagged-expr))
(define [tagged-expr-tag-eq? tagged-expr tag]
  [if [pair? tagged-expr]
    [eq? (tagged-expr-tag tagged-expr) tag]
    #f])

(define [quoted? tagged-expr] [tagged-expr-tag-eq? tagged-expr 'quote])
(define [make-quote-text text] (cons 'quote text))
(define [quote-text tagged-expr] (tagged-expr-body tagged-expr))

(define [if? tagged-expr] [tagged-expr-tag-eq? tagged-expr 'if])
(define [make-if-expr predicate consequent alternative]
  (cons 'if (cons predicate (cons consequent alternative))))
(define [if-predicate if-expr] (car (tagged-expr-body if-expr)))
(define [if-consequent if-expr] (cadr (tagged-expr-body if-expr)))
(define [if-alternative if-expr] (cddr (tagged-expr-body if-expr)))
(define [eval-if-expr if-expr env]
  (if [true? (evlt (if-predicate if-expr) env)]
    (evlt (if-consequent if-expr) env)
    (evlt (if-alternative if-expr) env)))

(define [when? tagged-expr] [tagged-expr-tag-eq? tagged-expr 'when])
(define [make-when-expr predicate consequent]
  (cons 'when (cons predicate consequent)))
(define [when-predicate when-expr] (car (tagged-expr-body when-expr)))
(define [when-consequent when-expr] (cdr (tagged-expr-body when-expr)))
(define [eval-when-expr when-expr env]
  (when [true? (evlt (when-predicate when-expr) env)]
    (evlt (when-consequent when-expr) env)))

(define [unless? tagged-expr] [tagged-expr-tag-eq? tagged-expr 'unless])
(define [make-unless-expr predicate alternative]
  (cons 'unless (cons predicate alternative)))
(define [unless-predicte unless-expr] (car (tagged-expr-body unless-expr)))
(define [unless-alternative unless-expr] (cdr (tagged-expr-body unless-expr)))
(define [eval-unless-expr unless-expr env]
  (unless [true? (evlt (unless-expr unless-expr) env)]
    (evlt (unless-alternative unless-expr) env)))

(define [make-clause predicate consequent] (cons predicate consequent))
(define [clause-predicate clause] (car clause))
(define [clause-consequent clause] (cdr clause))
(define [else-clause? clause] [eq? (clause-consequent clause) 'else])

(define [cond? tagged-expr] [tagged-expr-tag-eq? tagged-expr 'cond])
(define [make-cond-expr clauses] (cons 'cond clauses))
(define [cond-clauses cond-expr] (tagged-expr-body cond-expr))
(define [expand-clauses clauses]
  (define [expand remaining-clauses]
    (let* ([first-clause (car clauses)]
           [first-predicate (clause-predicate first-clause)]
           [first-consequent (clause-consequent first-clause)]
           [rest-clauses (cdr clauses)])
      (cond ([and [else-clause? first-clause] [null? rest-clauses]]
             (sequence->exprs first-consequent))
            ([and [else-clause? first-clause] [not [null? rest-clauses]]]
             (error "else clause followed by clause -- EXPAND-CLAUSES"
                    first-clause
                    rest-clauses))
            (else (make-if-expr first-predicate
                                (sequence->exprs first-consequent)
                                (expand-clauses rest-clauses))))))
  ;simulate the case when no predicates matched
  (cond ([pair? clauses] (expand clauses))))
(define [cond->ifs cond-expr]
  (expand-clauses (cond-clauses cond-expr)))
(define [eval-cond-expr])

;'define <var> <value>
;'define [<var> <param0> ...] body
;'define <var> 'lambda [<param0> ...] body
(define [lambda? tagged-expr] (tagged-expr-tag-eq? tagged-expr 'lambda))
(define [make-lambda-expr parameters body]
  (cons 'lambda (cons parameters body)))
(define [lambda-parameters tagged-expr]
  (let ([first-elt (car (tagged-expr-body tagged-expr))])
    (if [pair? first-elt]
      first-elt
      (error "invalid lambda expression -- LAMBDA-PARAMETERS" tagged-expr))))
(define [lambda-body tagged-expr]
  (let ([second-elt (cdr (tagged-expr-body tagged-expr))])
    (if [pair? second-elt]
      second-elt
      (error "invalid lambda expression -- LAMBDA-BODY" tagged-expr))))

(define [begin? tagged-expr] [tagged-expr-tag-eq? tagged-expr 'begin])
(define [make-begin-expr . exprs] (cons 'begin exprs))
(define [begin-inner-exprs begin-expr] (tagged-expr-body begin-expr))
(define [begin-first-expr inner-exprs] (car inner-exprs))
(define [begin-rest-exprs inner-exprs] (cdr inner-exprs))
(define [begin-last-expr? inner-exprs] [null? (begin-rest-exprs inner-exprs)])
(define [eval-begin-expr begin-expr])

(define [sequence->exprs sequence]
  (cond ([null? sequence] sequence)
        ([null? (cdr sequence)] (car sequence))
        (else (make-begin-expr sequence))))
(define [eval-sequence exprs env]
  (if [last-expr? exprs]
    (evlt (first-expr exprs) env)
    (begin
      (evlt (first-expr exprs) env)
      (eval-sequence (rest-exprs exprs) env))))

;'set! <var> <value>
(define [assignment? tagged-expr] (tagged-expr-tag-eq? tagged-expr 'set!))
(define [make-assignment-expr variable new-value]
  (cons 'set! (cons variable new-value)))
(define [assignment-variable assignment-expr]
  (car (tagged-expr-body assignment-expr)))
(define [assignment-value expr]
  (cdr (tagged-expr-body assignment-expr)))
(define [set-variable-value! variable new-value env])
(define [eval-assignment-expr assignment-expr env]
  (set-variable-value! (assignment-variable assignment-expr)
                       (evlt (assignment-value assignment-expr) env)
                       env)
  'assignment-ok)

;two case, variable with initial value and variable with lambda expression
(define [definition? tagged-expr] (tagged-expr-tag-eq? tagged-expr 'define))
(define [make-definition-expr variable expr]
  (cond ([and [symbol? variable] [lambda? expr]]
         (cons 'define (cons variable expr)))
        ([symbol? variable]
         (cons 'define (cons variable expr)))
        (else (error "invalid aruguments -- MAKE-DEFINITION-EXPR"
                     variable
                     expr))))
(define [definition-variable definition-expr]
  (let ([body-first-elt (car (tagged-expr-body definition-expr))])
    (cond ([symbol? body-first-elt] body-first-elt)
          ([pair? body-first-elt] (car body-first-elt))
          (else (error "invalid define expression -- DEFINITION-VARIABLE"
                       definition-expr)))))
(define [definition-value definition-expr]
  (let ([body-first-elt (car (tagged-expr-body definition-expr))]
        [body-left-elts (cdr (tagged-expr-body definition-expr))])
    (cond ([symbol? body-first-elt] tagged-expr-body)
          ([pair? body-first-elt]
           (make-lambda-expr (cdr body-first-elt)
                        (body-left-elts)))
          (else (error "invalid define expression -- DEFINITION-VALUE"
                       definition-expr)))))
(define [define-variable! variable init-value env])
(define [eval-definition-expr expr env]
  (define-variable! (definition-variable expr)
                    (evlt (definition-value expr) env)
                    env)
  'definition-ok)

(define [application? expr] [pair? expr])
(define [operator expr] (car expr))
(define [operands expr] (cdr expr))
(define [no-operand? operands] [null? operands])
(define [first-operand operands] (car operands))
(define [rest-operands operands] (cdr operands))
(define [list-of-values operands env]
  (if [no-operand? operands]
    '()
    (cons (evlt (first-operand operands) env)
          (list-of-values (rest-operands operands) env))))

(define [evlt expr env]
  (cond ([self-evaluating? expr] expr)
        ([variable? expr] (lookup-variable-value expr env))
        ([quoted? expr] (quote-text expr))
        ([assignment? expr] (eval-assignment-expr expr env))
        ([definition? expr] (eval-definition-expr expr env))
        ([if? expr] (eval-if-expr expr env))
        ([when? expr] (eval-when-expr expr env))
        ([unless? expr] (eval-unless-expr expr env))
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

;answer: it say that to re-arrange cond predicates sequence, not cond predicate
;itself. So when met (define x 3), 'define is treated as a procedure symbol to 
;be called. but in env, there is no define symbol defined. Things not work.
