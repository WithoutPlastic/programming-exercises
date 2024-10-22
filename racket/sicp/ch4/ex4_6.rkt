#lang racket

(define tagged-expr-eval-table (make-hash))
(define [register-tagged-expr-eval tag proc]
  (hash-set! tagged-expr-eval-table tag proc))
(define [search-tag-proc tag]
  (hash-ref tagged-expr-eval-table tag))

;with high priority, non-self-evaluating and non-variable expr indicate a

(define [true? expr])
(define true)
(define [false? expr])
(define false)
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
(define [tagged-expr? expr]
  [if [pair? expr]
    [hash-has-key? tagged-expr-eval-table
                   (tagged-expr-tag expr)]
    #f])

(define [quoted? tagged-expr] [tagged-expr-tag-eq? tagged-expr 'quote])
(define [make-quote-text text] (cons 'quote text))
(define [quote-text tagged-expr] (tagged-expr-body tagged-expr))
(register-tagged-expr-eval 'quote quote-text)

(define [and? tagged-expr] [tagged-expr-tag-eq? tagged-expr 'and])
(define [make-and-expr cond-exprs] (cons 'and cond-exprs))
(define [and-cond-exprs and-expr] (cdr and-expr))
(define [eval-and-expr and-expr env]
  (define [iter remaining-cond-exprs]
    (let* ([first-cond-expr (car remaining-cond-exprs)]
           [rest-cond-exprs (cdr remaining-cond-exprs)]
           [first-cond-return (evlt first-cond-expr env)])
      (cond ([false? first-cond-return] false)
            ([null? rest-cond-exprs] first-cond-return)
            (else (iter rest-cond-exprs)))))

  (if [null? (and-cond-exprs and-expr)]
    true
    (iter (and-cond-exprs and-expr))))
(register-tagged-expr-eval 'and eval-and-expr)

(define [or? tagged-expr] [tagged-expr-tag-eq? tagged-expr 'or])
(define [make-or-expr cond-exprs] (cons 'or cond-exprs))
(define [or-cond-exprs or-expr] (cdr or-expr))
(define [eval-or-expr or-expr env]
  (define [iter remaining-cond-exprs]
    (let* ([first-cond-expr (car remaining-cond-exprs)]
           [rest-cond-exprs (cdr remaining-cond-exprs)]
           [first-cond-return (evlt first-cond-expr env)])
      (cond ([true? first-cond-return] true)
            ([null? rest-cond-exprs] false)
            (else (iter rest-cond-exprs)))))
  
  (if [null? (or-cond-exprs or-expr)]
    false
    (iter (or-cond-exprs or-expr))))
(register-tagged-expr-eval 'or eval-or-expr)

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
(register-tagged-expr-eval 'if eval-if-expr)

(define [when? tagged-expr] [tagged-expr-tag-eq? tagged-expr 'when])
(define [make-when-expr predicate consequent]
  (cons 'when (cons predicate consequent)))
(define [when-predicate when-expr] (car (tagged-expr-body when-expr)))
(define [when-consequent when-expr] (cdr (tagged-expr-body when-expr)))
(define [eval-when-expr when-expr env]
  (when [true? (evlt (when-predicate when-expr) env)]
    (evlt (when-consequent when-expr) env)))
(register-tagged-expr-eval 'when eval-when-expr)

(define [unless? tagged-expr] [tagged-expr-tag-eq? tagged-expr 'unless])
(define [make-unless-expr predicate alternative]
  (cons 'unless (cons predicate alternative)))
(define [unless-predicte unless-expr] (car (tagged-expr-body unless-expr)))
(define [unless-alternative unless-expr] (cdr (tagged-expr-body unless-expr)))
(define [eval-unless-expr unless-expr env]
  (unless [true? (evlt (unless-expr unless-expr) env)]
    (evlt (unless-alternative unless-expr) env)))
(register-tagged-expr-eval 'unless eval-unless-expr)

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
(define [eval-cond-expr cond-expr env]
  (eval-if-expr (cond->ifs cond-expr) env))
(register-tagged-expr-eval 'cond eval-cond-expr)

;'define <var> <value>
;'define [<var> <param0> ...] body
;'define <var> 'lambda [<param0> ...] body
(define [lambda? tagged-expr] [tagged-expr-tag-eq? tagged-expr 'lambda])
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
(define [eval-lambda-expr lambda-expr env]
  )
(register-tagged-expr-eval 'lambda eval-lambda-expr)

(define [make-arg-pair variable expr] (cons variable expr))
(define [arg-pair-variable arg-pair] (car arg-pair))
(define [arg-pair-expr arg-pair] (cdr arg-pair))

(define [let? tagged-expr] [tagged-expr-tag-eq? tagged-expr 'let])
(define [make-let-expr arg-pairs sequence]
  (cons 'let (cons arg-pairs sequence)))
(define [let-expr-arg-pairs let-expr] (cadr let-expr))
(define [let-expr-sequence let-expr] (cddr let-expr))
(define [let->combination let-expr]
  (cons (make-lambda-expr (map arg-pair-variable (let-expr-arg-pairs let-expr))
                          (let-expr-sequence let-expr))
        (map arg-pair-expr (let-expr-arg-pairs let-expr))))
(define [eval-let-expr let-expr env]
  (evlt (let->combination let-expr) env))
(register-tagged-expr-eval 'let eval-let-expr)

(define [begin? tagged-expr] [tagged-expr-tag-eq? tagged-expr 'begin])
(define [make-begin-expr . exprs] (cons 'begin exprs))
(define [begin-inner-exprs begin-expr] (tagged-expr-body begin-expr))
(define [begin-first-expr inner-exprs] (car inner-exprs))
(define [begin-rest-exprs inner-exprs] (cdr inner-exprs))
(define [begin-last-expr? inner-exprs] [null? (begin-rest-exprs inner-exprs)])
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
(define [eval-begin-expr begin-expr env]
  (eval-sequence (begin-inner-exprs begin-expr) env))
(register-tagged-expr-eval 'begin eval-begin-expr)

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
(register-tagged-expr-eval 'set! eval-assignment-expr)

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
(register-tagged-expr-eval 'define eval-definition-expr)

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
        ([tagged-expr? expr]
         ((search-tag-proc (tagged-expr-tag expr)) expr env))
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

