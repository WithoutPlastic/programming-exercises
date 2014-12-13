#lang racket

;Frame in environment operation
;(list <var val> ...)
(define [make-frame vv-pairs] vv-pairs)
(define [frame-variables frame] (map car frame))
(define [frame-values frame] (map cdr frame))
(define [first-pair vv-pairs] (car vv-pairs))
(define [rest-pairs vv-pairs] (cdr vv-pairs))
(define [add-binding-to-frame variable value frame]
  (cons (cons variable value) frame))

;Variable value pair, same to pa-pairs
(define [make-vv-pair variable value] (cons variable value))
(define [vv-pair-variable vv-pair] (car vv-pair))
(define [vv-pair-value vv-pair] (cdr vv-pair))

;Param arg pair, same to vv-pairs
(define make-pa-pair make-vv-pair)
(define pa-pair-parameter vv-pair-variable)
(define pa-pair-argument vv-pair-value)

;Environment
;(list <frame> ...)
(define the-empty-environment '())
(define [first-frame env] (car env))
(define [rest-frames env] (cdr env))
(define enclosing-environment rest-frames)
(define [extend-environment vv-pairs base-env]
  (cons (make-frame vv-pairs) base-env))
(define [env-variable-operation variable env founded-proc not-founded-proc]
  (define deepest-frame (first-frame env))
  (define [frame-iter enclosing-env]
    (if [eq? enclosing-env the-empty-environment]
      (not-founded-proc deepest-frame)
      (inner-frame-iter (first-frame enclosing-env))))
  (define [inner-frame-iter vv-pairs]
    (cond ([null? vv-pairs]
           (frame-iter (enclosing-environment env)))
          ([eq? (car (first-pair vv-pairs)) variable]
           (founded-proc (first-pair vv-pairs)))
          (else (inner-frame-iter (rest-pairs vv-pairs)))))
  
  (frame-iter env))
(define [lookup-variable-value variable env]
  (env-variable-operation
    variable
    env
    (lambda [pair] (cdr pair))
    (lambda [frame] (error "variable not founded -- LOOKUP-VARIABLE-VALUE"
                           variable))))
(define [set-variable-value! variable new-value env]
  (env-variable-operation
    variable
    env
    (lambda [pair] (set-cdr! pair new-value))
    (lambda [frame] (error "variable not founded -- SET-VARIABLE-VALUE!"
                           variable))))
(define [define-variable! variable init-value env]
  (env-variable-operation
    variable
    env
    (lambda [pair] (error "duplicated variable definition -- DEFINE-VARIABLE!"
                          variable))
    (lambda [frame] (add-binding-to-frame frame))))
(define [setup-environment]
  (let ([init-env (extend-environment primitive-table the-empty-environment)])
    (define-variable! 'true #t init-env)
    (define-variable! 'false #f init-env)
    (define-variable! '#t #t init-env)
    (define-variable! '#f #f init-env)
    (define-variable! 'void void init-env)
    init-env))

;Only deepest frame should be operate by make-unbound! To modify other frame
;will increase complex and make security risk.
(define [make-unbound-expr variable] (list 'unbound! variable))
(define [unbound? tagged-expr] [tagged-expr-tag-eq? tagged-expr 'unbound!])
(define [unbound-variable unbound-expr] (cadr unbound-expr))
(define [eval-unbound! unbound-expr env]
  (define [not-founded-error]
      (error "variable not founded -- MAKE-UNBOUND!"
             (unbound-variable unbound-expr)))
  (define [inner-frame-iter vv-pairs]
    (cond ([null? vv-pairs] (not-founded-error))
          ([eq? (car (first-pair vv-pairs)) variable]
           (set-car! (first-pair vv-pairs) '())
           (set-cdr! (first-pair vv-pairs) '()))
          (else (inner-frame-iter (rest-pairs vv-pairs)))))
  (inner-frame-iter (first-frame env)))

;Self evaluation elements
;"hello world"
;123
(define [self-evaluating? elt] [or [number? elt] [string? elt]])

;Those symbol is presented with ('quote x)
;Individual symbol indicate a variable in environment
;'foo
(define [variable? elt] [symbol? elt])

;Define tagged expr lookup table and its register, lookup interface
(define tagged-expr-eval-table (make-hash))
(define [register-tagged-expr-eval tag eval-proc]
  (hash-set! tagged-expr-eval-table tag eval-proc))
(define [search-tag-expr-eval tag] (hash-ref tagged-expr-eval-table tag))

;Tagged-expr processing
(define [tagged-expr-tag tagged-expr] (car tagged-expr))
(define [tagged-expr-body tagged-expr] (cdr tagged-expr))
(define [tagged-expr? elt]
  [and [pair? elt]
       [hash-has-key? tagged-expr-eval-table (tagged-expr-tag elt)]])
(define [tagged-expr-tag-eq? tagged-expr tag]
  [eq? (tagged-expr-tag tagged-expr) tag])

;Quote tag expr
;('quote text)
(define [make-quote-tag-expr text] (list 'quote text))
(define [quote-expr? tagged-expr] [tagged-expr-tag-eq? tagged-expr 'quote])
(define [quote-text tagged-expr] (car (tagged-expr-body tagged-expr)))
(define eval-quote-expr quote-text)
(register-tagged-expr-eval 'quote eval-quote-expr)

;And tag expr
;('and <expr> ...)
(define [make-and-expr cond-exprs] (cons 'and cond-exprs))
(define [and-expr? tagged-expr] [tagged-expr-tag-eq? tagged-expr 'and])
(define [and-cond-exprs and-expr] (tagged-expr-body and-expr))
(define [eval-and-expr and-expr env]
  (define [iter remaining-cond-exprs]
    (let* ([first-cond-expr (car remaining-cond-exprs)]
           [rest-cond-exprs (cdr remaining-cond-exprs)]
           [first-cond-return (evlt first-cond-expr env)])
      (cond (first-cond-return #f)
            ([null? rest-cond-exprs] first-cond-return)
            (else (iter rest-cond-exprs)))))

  (if [null? (and-cond-exprs and-expr)]
    #t
    (iter (and-cond-exprs and-expr))))
(register-tagged-expr-eval 'and eval-and-expr)

;Or tag expr
;('or <expr> ...)
(define [make-or-expr cond-exprs] (cons 'or cond-exprs))
(define [or? tagged-expr] [tagged-expr-tag-eq? tagged-expr 'or])
(define [or-cond-exprs or-expr] (tagged-expr-body or-expr))
(define [eval-or-expr or-expr env]
  (define [iter remaining-cond-exprs]
    (let* ([first-cond-expr (car remaining-cond-exprs)]
           [rest-cond-exprs (cdr remaining-cond-exprs)]
           [first-cond-return (evlt first-cond-expr env)])
      (cond (first-cond-return #t)
            ([null? rest-cond-exprs] #f)
            (else (iter rest-cond-exprs)))))
  
  (if [null? (or-cond-exprs or-expr)]
    #f
    (iter (or-cond-exprs or-expr))))
(register-tagged-expr-eval 'or eval-or-expr)

;If tag expr
;('if <predicate> <consequent> <alternative>)
(define [make-if-expr predicate consequent alternative]
  (list 'if predicate consequent alternative))
(define [if? tagged-expr] [tagged-expr-tag-eq? tagged-expr 'if])
(define [if-predicate if-expr] (car (tagged-expr-body if-expr)))
(define [if-consequent if-expr] (cadr (tagged-expr-body if-expr)))
(define [if-alternative if-expr] (caddr (tagged-expr-body if-expr)))
(define [eval-if-expr if-expr env]
  (if (evlt (if-predicate if-expr) env)
    (evlt (if-consequent if-expr) env)
    (evlt (if-alternative if-expr) env)))
(register-tagged-expr-eval 'if eval-if-expr)

;When tag expr
;('when <predicate> <consequent>)
(define [make-when-expr predicate consequent]
  (list 'when predicate consequent))
(define [when? tagged-expr] [tagged-expr-tag-eq? tagged-expr 'when])
(define [when-predicate when-expr] (car (tagged-expr-body when-expr)))
(define [when-consequent when-expr] (cadr (tagged-expr-body when-expr)))
(define [eval-when-expr when-expr env]
  (when (evlt (when-predicate when-expr) env)
    (evlt (when-consequent when-expr) env)))
(register-tagged-expr-eval 'when eval-when-expr)

;Unless tag expr
;('unless <predicate> <alternative>)
(define [make-unless-expr predicate alternative]
  (cons 'unless (cons predicate alternative)))
(define [unless? tagged-expr] [tagged-expr-tag-eq? tagged-expr 'unless])
(define [unless-predicte unless-expr] (car (tagged-expr-body unless-expr)))
(define [unless-alternative unless-expr] (cadr (tagged-expr-body unless-expr)))
(define [eval-unless-expr unless-expr env]
  (unless (evlt (unless-predicte unless-expr) env)
    (evlt (unless-alternative unless-expr) env)))
(register-tagged-expr-eval 'unless eval-unless-expr)

;Cond clause , nested in cond tag expr
;(<cond> <expr> ...)
(define [make-clause predicate consequents] (cons predicate consequents))
(define [clause-predicate clause] (car clause))
(define [clause-consequents clause] (cdr clause))
;(#t <expr> ...)
(define [else-clause? clause] [eq? (clause-predicate clause) 'else])
;Cond tag expr
;('cond ((<cond> <expr> ...) ...))
(define [make-cond-expr clauses] (cons 'cond clauses))
(define [cond? tagged-expr] [tagged-expr-tag-eq? tagged-expr 'cond])
(define [cond-clauses cond-expr] (tagged-expr-body cond-expr))
(define [cond->nested-if-exprs cond-expr]
  (define [expand-clauses clauses]
    (define [expand]
      (let ([first-clause (car clauses)]
            [rest-clause (cdr clauses)])
        (if [and [else-clause? first-clause] [not [null? rest-clause]]]
          (error "cond else clause followed by clause -- EXPAND-CLAUSES")
          (make-if-expr (if [else-clause? first-clause]
                          '#t
                          (clause-predicate first-clause))
                        (clause-consequent first-clause)
                        (expand-clauses (cdr clauses))))))

    (if [null? clauses] (void) (expand)))

  (expand-clauses (cond-clauses cond-expr)))
(define [eval-cond-expr cond-expr env]
  (eval-if-expr (cond->nested-if-exprs cond-expr) env))
(register-tagged-expr-eval 'cond eval-cond-expr)

;Lambda tag expr
;('lambda (<param> ...) <expr> ...)
(define [make-lambda-expr parameters exprs]
  (if [null? exprs]
    (error "lambda body empty -- MAKE-LAMBDA-EXPR")
    (cons 'lambda (cons parameters exprs))))
(define [lambda? tagged-expr] [tagged-expr-tag-eq? tagged-expr 'lambda])
(define [lambda-parameters tagged-expr]
  (let ([parameters (car (tagged-expr-body tagged-expr))])
    (if [list? parameters]
      parameters
      (error "invalid lambda expression -- LAMBDA-PARAMETERS" parameters))))
(define [lambda-body tagged-expr]
  (let ([exprs (cdr (tagged-expr-body tagged-expr))])
    (if [pair? exprs]
      exprs
      (error "invalid lambda expression -- LAMBDA-BODY" exprs))))
(define [eval-lambda-expr lambda-expr env]
  (define [eval-exprs exprs new-env]
    (define [iter remaining-exprs]
      (let ([first-expr (car remaining-exprs)]
            [rest-exprs (cdr remaining-exprs)])
        (cond ([null? rest-exprs] (evlt first-expr new-env))
              (else (evlt first-expr new-env)
                    (iter rest-exprs)))))
    (iter exprs)

  (lambda args
    (if [= (length args) (length (lambda-parameters lambda-expr))]
      (eval-exprs (lambda-body lambda-expr)
                  (extend-environment
                    (map (lambda [var val] (cons var val))
                         (lambda-parameters lambda-expr)
                         args)
                    env))
      (error "parameters and arguments inconsist -- EVAL-LAMBDA-EXPR")))))
(register-tagged-expr-eval 'lambda eval-lambda-expr)

;Definition tag expr
;Default ('define <var> <value>)
;Default ('define <var> ('lambda [<param0> ...] exprs))
;('define [<var> <param0> ...] exprs)
;Non default style won't be generate by make expr
(define [make-definition-expr variable value]
  (if [symbol? variable]
    (list 'define variable value)
    (error "invalid variable -- MAKE-DEFINITION-EXPR" variable value)))
(define [definition? tagged-expr] [tagged-expr-tag-eq? tagged-expr 'define])
(define [definition-variable definition-expr]
  (define [proc-default-style first-elt] first-elt)
  (define [proc-non-default-style first-elt] (car first-elt))

  (let ([first-elt (car (tagged-expr-body definition-expr))])
    (cond ([symbol? first-elt] (proc-default-style first-elt))
          ([pair? first-elt] (proc-non-default-style first-elt))
          (else (error "invalid define expression -- DEFINITION-VARIABLE"
                       first-elt)))))
(define [definition-value definition-expr]
  (define [proc-default-style first-elt second-elt] second-elt)
  (define [proc-non-default-style first-elt second-elt]
    (make-lambda-expr (cdr first-elt) second-elt))

  (let ([first-elt (car (tagged-expr-body definition-expr))]
        [second-elt (cadr (tagged-expr-body definition-expr))])
    (cond ([symbol? first-elt] (proc-default-style first-elt second-elt))
          ([pair? first-elt] (proc-non-default-style first-elt second-elt))
          (else (error "invalid define expression -- DEFINITION-VALUE"
                       second-elt)))))
(define [eval-definition-expr definition-expr env]
  (define-variable! (definition-variable definition-expr)
                    (evlt (definition-value definition-expr) env)
                    env)
  'definition-ok)
(register-tagged-expr-eval 'define eval-definition-expr)

;Let tag expr
;('let (<var value> ...) exprs ...)
(define [make-let-expr pa-pairs exprs] (cons 'let (cons pa-pairs exprs)))
(define [let? tagged-expr] [tagged-expr-tag-eq? tagged-expr 'let])
(define [let-pa-pairs let-expr] (car (tagged-expr-body let-expr)))
(define [let-exprs let-expr] (cdr (tagged-expr-body let-expr)))
(define [let->lambda-expr-call let-expr]
  (cons (make-lambda-expr (map pa-pair-parameter (let-pa-pairs let-expr))
                          (let-exprs let-expr))
        (map pa-pair-value (let-pa-pairs let-expr))))
(define [eval-let-expr let-expr env]
  (evlt (let->lambda-expr-call let-expr) env))
(register-tagged-expr-eval 'let eval-let-expr)

;Named let tag expr
;('let name (<var value> ...) exprs ...)
(define [make-named-let-expr self pa-pairs exprs]
  (cons 'named-let (cons self (cons pa-pairs exprs))))
(define [named-let? tagged-expr] [tagged-expr-tag-eq? tagged-expr 'named-let])
(define [named-let-self named-let-expr]
  (car (tagged-expr-body named-let-expr)))
(define [named-let-pa-pairs named-let-expr]
  (cadr (tagged-expr-body named-let-expr)))
(define [named-let-exprs named-let-expr]
  (cddr (tagged-expr-body named-let-expr)))
(define [named-let->definition named-let-expr]
  (make-definition-expr
    (named-let-self named-let-expr)
    (make-lambda-expr
      (map pa-pair-parameter (named-let-pa-pairs named-let-expr))
      (named-let-exprs named-let-expr))))
(define [eval-named-let-expr named-let-expr env]
  (evlt (named-let->definition named-let-expr) env)
  (evlt (make-procedure (named-let-self named-let-expr)
                        (map pa-pair-value (named-let-pa-pairs named-let-expr)))
        env))
(register-tagged-expr-eval 'named-let eval-named-let-expr)

;Let* tag expr
;('let* (<var value> ...) exprs ...)
(define [make-let*-expr pa-pairs exprs] (cons 'let* (cons pa-pairs exprs)))
(define [let*? tagged-expr] [tagged-expr-tag-eq? tagged-expr 'let*])
(define [let*-expr-pa-pairs let*-expr] (cadr let*-expr))
(define [let*-expr-exprs let*-expr] (cddr let*-expr))
(define [let*->nested-lets let*-expr]
  (let ([pa-pairs (let*-expr-pa-pairs let*-expr)]
        [exprs (let*-expr-exprs let*-expr)])
    (if [null? pa-pairs]
      exprs
      (make-let-expr (list (car pa-pairs))
                     (let*->nested-let-exprs
                       (make-let*-expr (cdr pa-pairs) exprs)))))
(define [eval-let*-expr let*-expr env]
  (evlt (let*->nested-lets let*-expr) env))
(register-tagged-expr-eval 'let* eval-let*-expr)

;Begin tag expr
;('begin <expr> ...)
(define [make-begin-expr exprs]
  (if [null? exprs]
    (error "get a empty expr input -- MAKE-BEGIN-EXPR")
    (cons 'begin exprs)))
(define [begin? tagged-expr] [tagged-expr-tag-eq? tagged-expr 'begin])
(define [begin-exprs begin-expr] (tagged-expr-body begin-expr))
(define [eval-begin-exprs exprs env]
  (if [null? (cdr exprs)]
    (evlt (first-expr exprs) env)
    (begin
      (evlt (first-expr exprs) env)
      (eval-begin-exprs (rest-exprs exprs) env))))
(define [eval-begin-expr begin-expr env]
  (eval-begin-exprs (begin-exprs begin-expr) env))
(register-tagged-expr-eval 'begin eval-begin-expr)

;Set! tag expr
;('set! <variable> <value>)
(define [make-assignment-expr variable value]
  (cons 'set! variable value))
(define [assignment? tagged-expr] [tagged-expr-tag-eq? tagged-expr 'set!])
(define [assignment-variable assignment-expr]
  (car (tagged-expr-body assignment-expr)))
(define [assignment-value assignment-expr]
  (cadr (tagged-expr-body assignment-expr)))
(define [eval-assignment-expr assignment-expr env]
  (set-variable-value! (assignment-variable assignment-expr)
                       (evlt (assignment-value assignment-expr) env)
                       env)
  'assignment-ok)
(register-tagged-expr-eval 'set! eval-assignment-expr)

;While tag expr
;racket do-loop never used before, here I implement simpler while loop instead.
;('while <stop-expr> exprs)
(define [make-while-expr stop-expr exprs]
  (cons 'while (cons stop-expr exprs)))
(define [while? tagged-expr] [tagged-expr-tag-eq? tagged-expr 'while])
(define [while-stop-expr while-expr] (cadr while-expr))
(define [while-exprs while-expr] (cddr while-expr))
(define [while->tail-recur-lambda-call while-expr]
  (list
    (make-lambda-expr
      '()
      (make-definition-expr
        'self
        (make-lambda '()
                     (make-unless-expr (while-stop-expr while-expr)
                                       (while-exprs while-expr))))
      (list 'self))))
(define [eval-while-expr while-expr]
  (evlt (while->tail-recur-lambda-call while-expr) env))
(register-tagged-expr-eval 'while eval-while-expr)

;Primitive table for mapping uplayer racket to underlayer racket
(define primitive-table
  (list (cons 'car car)
        (cons 'cdr cdr)
        (cons 'cons cons)
        (cons 'null? null?)
        ;More here
        ))
(define primitive-names (map car primitive-table))
(define primitive-objects (map cdr primitive-table))

;Procedure
;(proc-name arg ...)
;(<proc-expr> arg ...)
;(define [make-procedure parameters body env]
;  (cons 'procedure (cons parameters (cons body env))))
;(define [procedure-parameters procedure-expr] (cadr procedure-expr))
;(define [procedure-body procedure-expr] (caddr procedure-expr))
;(define [procedure-environment procedure-expr] (cdddr procedure-expr))
(define [make-procedure variable args] (cons variable args))
(define [generic-procedure? expr]
  (and [pair? expr] [not [tagged-expr? expr]]))
(define [primitive-procedure? expr]
  (and [generic-procedure? expr] [memq (car expr) primitive-names]))
(define [compound-procedure? expr] [and [pair? expr] [pair? [car expr]]])
(define [user-procedure? expr]
  [and [generic-procedure? expr]
       [not [primitive-procedure? expr]]
       [not [compound-procedure? expr]]])

(define [proc-name proc] (car proc))
(define [proc-arguments proc] (cdr proc))
(define [arguments->values arguments env]
  (define [iter remaining-arguments]
    (if [null? remaining-arguments]
      '()
      (cons (evlt (car remaining-arguments) env)
            (iter (cdr remaining-arguments) env))))
  (iter arguments))

(define [evlt-exprs exprs env] (map (lambda [expr] (evlt expr env)) exprs))

;Generic evaluation 
(define [evlt expr env]
  (cond ([self-evaluating? expr] expr)
        ([variable? expr] (lookup-variable-value expr env))
        ([tagged-expr? expr]
         ((search-tag-expr-eval (tagged-expr-tag expr)) expr env))
        ([generic-procedure? expr]
         (aply (evlt (proc-name expr) env)
               (evlt-exprs (proc-arguments expr) env)))
        (else
          (error "unknown expression type -- EVLT" expr))))

(define [apply-primitive-procedure primitive args] (apply primitive args))
(define [apply-compound-procedure procedure args env] (apply procedure args))

;Generic apply
(define [aply procedure arguments]
  (cond ([memq procedure primitive-objects]
         (apply-primitive-procedure procedure arguments))
        (else
         (apply-compound-procedure procedure arguments)))
        ;(else
        ;  (error "unknown procedure type -- APLY" procedure))))

;Interactive evaluator
(define the-global-environment (setup-environment))
(define [prompt-for-input string]
  (newline) (newline) (display string) (newline))
(define [announce-output string]
  (newline) (display string) (newline))

(define [user-print object]
  (if [compound-procedure? object]
    (display (list 'compound-procedure
                   (procedure-parameters object)
                   (procedure-body object)
                   <proc-env>))
    (display object)))

(define [driver-loop]
  (define input-prompt ";;; M-Eval input:")
  (define output-prompt ";;; M-Eval value:")

  (prompt-for-input input-prompt)
  (let ([input (read)])
    (let ([output [evlt input the-global-environment]])
      (announce-output output-prompt)
      (user-print output)))
  driver-loop)
(driver-loop)

;;;; M-Eval input:
;(define [append x y]
;  (if [null? x]
;    y
;    (cons (car x)
;          (append (cdr x) y))))
;;;; M-Eval value:
;ok
;
;;;; M-Eval input;
;(append '(a b c) '(d e f))
;
;;;; M-Eval value:
;(a b c d e f)
