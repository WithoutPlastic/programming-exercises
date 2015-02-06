#lang racket

;Data structure of variable-value pair
;Data structure of parameter-argument pair
(define [make-vv-pair variable value] (mcons variable value))
(define [vv-pair-variable vv-pair] (mcar vv-pair))
(define [vv-pair-value vv-pair] (mcdr vv-pair))
(define [set-vv-pair-variable! vv-pair new-variable]
  (set-mcar! vv-pair new-variable))
(define [set-vv-pair-value! vv-pair new-value]
  (set-mcdr! vv-pair new-value))
(define the-empty-vv-pair (mcons '() '()))

(define [make-pa-pair param arg] (list param arg))
(define [pa-pair-parameter pa-pair] (car pa-pair))
(define [pa-pair-argument pa-pair] (cadr pa-pair))
(define the-empty-pa-pair (list '() '()))

;Frame is a chain list of vv-pair
(define the-empty-frame (mcons 'frame '()))
(define [get-empty-frame] (mcons 'frame '()))
(define [frame? frame] [and [mpair? frame] [eq? (mcar frame) 'frame]])
(define [frame-payload frame] (mcdr frame))
(define [empty-frame? frame]
  [and [frame? frame] [eq? (frame-payload frame) '()]])
(define [make-frame vv-pairs]
  (define [iter remaining-pairs]
    (if [null? remaining-pairs]
      '()
      (mcons (car remaining-pairs) (iter (cdr remaining-pairs)))))
  (mcons 'frame (iter vv-pairs)))
(define [frame-map proc frame]
  (define [iter remaining]
    (if [empty-frame? remaining]
      '()
      (cons (proc (frame-head remaining)) (iter (frame-rest remaining)))))
  (iter frame))
(define [frame-variables frame] (frame-map vv-pair-variable frame))
(define [frame-values frame] (frame-map vv-pair-value frame))
(define [frame-head frame] (mcar (frame-payload frame)))
(define [frame-rest frame] (mcons 'frame (mcdr (frame-payload frame))))
(define [frame-copy frame]
  (make-frame (map make-vv-pair (frame-variables frame) (frame-values frame))))
(define [frame-search frame variable]
  (define [iter remaining]
    (cond ([empty-frame? remaining] #f)
          ([eq? (vv-pair-variable (frame-head remaining)) variable]
           (frame-head remaining))
          (else (iter (frame-rest remaining)))))
  (iter frame))
(define [set-frame-payload! frame new-value] (set-mcdr! frame new-value))
(define [set-frame-head! frame new-value] (set-mcar! (frame-payload frame) new-value))
(define [set-frame-rest! frame new-value] (set-mcdr! (frame-payload frame) new-value))
(define [frame-add! frame vv-pair]
  (set-frame-payload! frame (mcons vv-pair (frame-payload frame))))
(define [frame-remove! frame variable]
  (define [iter last-node remaining]
    (cond ([empty-frame? remaining]
           (error "remove variable failed, not founded -- FRAME-REMOVE!"))
          ([eq? (vv-pair-variable (frame-head remaining)) variable]
           (set-mcdr! last-node (frame-payload (frame-rest remaining))))
          (else (iter (mcdr last-node) (frame-rest remaining)))))
  (iter frame frame))
(define [add-binding-to-frame! frame variable value]
  (frame-add! frame (make-vv-pair variable value)))
(define [remove-binding-from-frame! frame variable] (frame-remove! frame variable))

;Environment
;(chained <frame> ...)
(define the-empty-environment (mcons 'environment '()))
(define [environment-payload env] (mcdr env))
(define [environment? env] [and [mpair? env] [eq? (mcar env) 'environment]])
(define [empty-environment? env]
  [and [environment? env] [eq? (environment-payload env) '()]])
(define [env-head env]
  (cond ([not [environment? env]]
         (error "given a non environment -- ENV-HEAD"))
        ([empty-environment? env]
         (error "access head of a empty environment -- ENV-HEAD"))
        (else (mcar (environment-payload env)))))
(define current-frame env-head)
(define [env-rest env]
  (cond ([not [environment? env]]
         (error "given a non environment -- ENV-REST"))
        ([empty-environment? env]
         (error "access rest of a empty environment -- ENV-REST"))
        (else (mcons 'environment (mcdr (environment-payload env))))))
(define enclosing-environment env-rest)
(define [extend-environment base-env vv-pairs]
  (if [environment? base-env]
    (mcons 'environment
           (mcons (make-frame vv-pairs)
                  (environment-payload base-env)))
    (error "given a non environment -- EXTEND-ENVIRONMENT")))
(define [env-variable-operation env variable founded-proc not-founded-proc]
  (define cframe (current-frame env))
  (define [iter enclosing-env]
    (define [frame-iter remaining-frame]
      (cond ([empty-frame? remaining-frame]
             (iter (enclosing-environment enclosing-env)))
            ([eq? (vv-pair-variable (frame-head remaining-frame)) variable]
             (founded-proc cframe (frame-head remaining-frame)))
            (else (frame-iter (frame-rest remaining-frame)))))
    (if [empty-environment? enclosing-env]
      (not-founded-proc cframe env)
      (frame-iter (env-head enclosing-env))))
  (if [environment? env]
    (iter env)
    (error "given a non environment -- ENV-VARIABLE-OPERATION")))
(define [lookup-variable-value env variable]
  (env-variable-operation
    env
    variable
    (lambda [frame pair] (vv-pair-value pair))
    (lambda [frame env] (error "variable not founded -- LOOKUP-VARIABLE-VALUE"
                           variable))))
(define [set-variable-value! env variable new-value]
  (env-variable-operation
    env
    variable
    (lambda [frame pair] (set-vv-pair-value! pair new-value))
    (lambda [frame env] (error "variable not founded -- SET-VARIABLE-VALUE!"
                           variable))))
(define [define-variable! env variable init-value]
  (env-variable-operation
    env
    variable
    (lambda [frame pair] (add-binding-to-frame! frame variable init-value))
      ;(error "duplicated variable definition -- DEFINE-VARIABLE!" variable))
    (lambda [frame env] (add-binding-to-frame! frame variable init-value))))

;Thunk object manipulation
(define [make-thunk expr env]
  (if [environment? env]
    (mcons 'thunk (mcons expr env))
    (error "given a non environment -- MAKE-THUNK")))
(define [thunk? object] [and [mpair? object] [eq? (mcar object) 'thunk]])
(define [thunk-expr thunk] (mcar (mcdr thunk)))
(define [thunk-env thunk] (mcdr (mcdr thunk)))
(define [thunk->evaluated! tk]
  (let ([value (force-it (evlt (thunk-expr tk) (thunk-env tk)))])
    (set-mcar! tk 'evaluated-thunk)
    (set-mcar! (mcdr tk) value)
    (set-mcdr! (mcdr tk) '())
    value))
(define [evaluated-thunk? object]
  [and [mpair? object] [eq? (mcar object) 'evaluated-thunk]])
(define [evaluated-thunk-value evaluted-thunk] (mcar (mcdr evaluted-thunk)))
(define [delay-it object env]
  (cond ([not [environment? env]] (error "given a non environment -- DELAY-IT"))
        ([thunk? object] object)
        (else (make-thunk object env))))
(define [force-it object]
  (cond ([thunk? object] (thunk->evaluated! object))
        ([evaluated-thunk? object] (evaluated-thunk-value object))
        (else object)))

;Define input->tagged-expr proc table
(define input->tagged-expr-list '())
(define [add-input->tagged-expr-proc proc]
  (set! input->tagged-expr-list (cons proc input->tagged-expr-list )))
(define [input->tagged-expr input]
  [ormap (lambda [x] (x input)) 
         (append (filter (lambda [x] [not [eq? x input->procedure-expr]])
                         input->tagged-expr-list)
                 (filter (lambda [x] [eq? x input->procedure-expr])
                         input->tagged-expr-list))])

;Define tagged expr analyze proc table
(define tagged-expr-analyze-table (make-hash))
(define [register-tagged-expr-analyze tag analyze-proc]
  (hash-set! tagged-expr-analyze-table tag analyze-proc))
(define [get-tagged-expr-analyze tag] (hash-ref tagged-expr-analyze-table tag))

;Define tagged expr eval proc table
(define tagged-expr-eval-table (make-hash))
(define [register-tagged-expr-eval tag eval-proc]
  (hash-set! tagged-expr-eval-table tag eval-proc))
(define [get-tagged-expr-eval tag] (hash-ref tagged-expr-eval-table tag))

;Tagged expr processing
(define [tagged-expr-tag tagged-expr] (car tagged-expr))
(define [tagged-expr-body tagged-expr] (cdr tagged-expr))
(define [tagged-expr? elt]
  [and [pair? elt] [symbol? (car elt)]])
(define [tagged-expr-tag-eq? tagged-expr tag]
  [and [tagged-expr? tagged-expr] [eq? (tagged-expr-tag tagged-expr) tag]])

;Boolean test
(define [is-false? object] [eq? object false])
(define [is-true? object] [not [is-false? object]])

;Number expr
(define [make-number-expr number] (cons 'number-expr number))
(define [number-expr? tagged-expr] [tagged-expr-tag-eq? tagged-expr 'number-expr])
(define [number-expr-value number-expr] (tagged-expr-body number-expr))
(define [input->number-expr input]
  [and [number? input] (make-number-expr input)])
(add-input->tagged-expr-proc input->number-expr)
(define [analyze-number-expr number-expr] number-expr)
(register-tagged-expr-analyze 'number-expr analyze-number-expr)
(define [eval-number-expr analyzed-number-expr env]
  (number-expr-value analyzed-number-expr))
(register-tagged-expr-eval 'number-expr eval-number-expr)

;String expr
(define [make-string-expr str] (cons 'string-expr str))
(define [string-expr? tagged-expr] [tagged-expr-tag-eq? tagged-expr 'string-expr])
(define [string-expr-value string-expr] (tagged-expr-body string-expr))
(define [input->string-expr input]
  [and [string? input] (make-string-expr input)])
(add-input->tagged-expr-proc input->string-expr)
(define [analyze-string-expr string-expr] string-expr)
(register-tagged-expr-analyze 'string-expr analyze-string-expr)
(define [eval-string-expr analyzed-string-expr env]
  (string-expr-value analyzed-string-expr))
(register-tagged-expr-eval 'string-expr eval-string-expr)

;Quote expr
(define [make-quote-expr texts] (cons 'quote-expr texts))
(define [quote-expr? tagged-expr] [tagged-expr-tag-eq? tagged-expr 'quote-expr])
(define [quote-expr-text quote-expr] (tagged-expr-body quote-expr))
(define [input->quote-expr input]
  [and [list? input] [= (length input) 2] [eq? (car input) 'quote]
       (make-quote-expr (cadr input))])
(add-input->tagged-expr-proc input->quote-expr)
(define [analyze-quote-expr quote-expr] quote-expr)
(register-tagged-expr-analyze 'quote-expr analyze-quote-expr)
(define [eval-quote-expr analyzed-quote-expr env]
  (quote-expr-text analyzed-quote-expr))
(register-tagged-expr-eval 'quote-expr eval-quote-expr)

;Variable expr
(define [make-variable-expr symbol] (cons 'variable-expr symbol))
(define [variable-expr? tagged-expr] [tagged-expr-tag-eq? tagged-expr 'variable-expr])
(define [variable-expr-symbol variable-expr] (tagged-expr-body variable-expr))
(define [input->variable-expr input]
  [and [symbol? input] (make-variable-expr input)])
(add-input->tagged-expr-proc input->variable-expr)
(define [analyze-variable-expr variable-expr] variable-expr)
(register-tagged-expr-analyze 'variable-expr analyze-variable-expr)
(define [eval-variable-expr analyzed-variable-expr env]
  (lookup-variable-value env (variable-expr-symbol analyzed-variable-expr)))
(register-tagged-expr-eval 'variable-expr eval-variable-expr)

;And expr
(define [make-and-expr cdts] (cons 'and-expr cdts))
(define [and-expr? tagged-expr] [tagged-expr-tag-eq? tagged-expr 'and-expr])
(define [and-expr-cond-exprs and-expr] (tagged-expr-body and-expr))
(define [input->and-expr input]
  [and [list? input] [not [null? input]] [eq? (car input) 'and]
       (make-and-expr (map input->tagged-expr (cdr input)))])
(add-input->tagged-expr-proc input->and-expr)
(define [analyze-and-expr and-expr]
  (let ([analyzed-conds (map analyze (and-expr-cond-exprs and-expr))])
    (make-and-expr analyzed-conds)))
(register-tagged-expr-analyze 'and-expr analyze-and-expr)
(define [eval-and-expr analyzed-and-expr env]
  [andmap (lambda [cdt] [is-true? (force-it (evlt cdt env))])
          (and-expr-cond-exprs analyzed-and-expr)])
(register-tagged-expr-eval 'and-expr eval-and-expr)

;Or expr
(define [make-or-expr cdts] (cons 'or-expr cdts))
(define [or-expr? tagged-expr] [tagged-expr-tag-eq? tagged-expr 'or-expr])
(define [or-expr-cond-exprs or-expr] (tagged-expr-body or-expr))
(define [input->or-expr input]
  [and [list? input] [not [null? input]] [eq? (car input) 'or]
       (make-or-expr (map input->tagged-expr (cdr input)))])
(add-input->tagged-expr-proc input->or-expr)
(define [analyze-or-expr or-expr]
  (let ([analyzed-conds (map analyze (or-expr-cond-exprs or-expr))])
    (make-or-expr analyzed-conds)))
(register-tagged-expr-analyze 'or-expr analyze-or-expr)
(define [eval-or-expr analyzed-or-expr env]
  [ormap (lambda [cdt] [is-true? (force-it (evlt cdt env))])
         (or-expr-cond-exprs analyzed-or-expr)])
(register-tagged-expr-eval 'or-expr eval-or-expr)

;If expr
(define [make-if-expr predicate consequent alternative]
  (cons 'if-expr (cons predicate (cons consequent alternative))))
(define [if-expr? tagged-expr] [tagged-expr-tag-eq? tagged-expr 'if-expr])
(define [if-expr-predicate if-expr] (car (tagged-expr-body if-expr)))
(define [if-expr-consequent if-expr] (cadr (tagged-expr-body if-expr)))
(define [if-expr-alternative if-expr] (cddr (tagged-expr-body if-expr)))
(define [input->if-expr input]
  [and [list? input] [= (length input) 4] [eq? (car input) 'if]
       (make-if-expr (input->tagged-expr (cadr input))
                     (input->tagged-expr (caddr input))
                     (input->tagged-expr (cadddr input)))])
(add-input->tagged-expr-proc input->if-expr)
(define [analyze-if-expr if-expr]
  (let ([predicate (analyze (if-expr-predicate if-expr))]
        [consequent (analyze (if-expr-consequent if-expr))]
        [alternative (analyze (if-expr-alternative if-expr))])
    (make-if-expr predicate consequent alternative)))
(register-tagged-expr-analyze 'if-expr analyze-if-expr)
(define [eval-if-expr analyzed-if-expr env]
  (if [is-true? (force-it (evlt (if-expr-predicate analyzed-if-expr) env))]
    (evlt (if-expr-consequent analyzed-if-expr) env)
    (evlt (if-expr-alternative analyzed-if-expr) env)))
(register-tagged-expr-eval 'if-expr eval-if-expr)

;When expr
(define [make-when-expr predicate consequent]
  (cons 'when-expr (cons predicate consequent)))
(define [when-expr? tagged-expr] [tagged-expr-tag-eq? tagged-expr 'when-expr])
(define [when-expr-predicate when-expr] (car (tagged-expr-body when-expr)))
(define [when-expr-consequent when-expr] (cdr (tagged-expr-body when-expr)))
(define [input->when-expr input]
  [and [list? input] [= (length input) 3] [eq? (car input) 'when]
       (make-when-expr (input->tagged-expr (cadr input))
                       (input->tagged-expr (caddr input)))])
(add-input->tagged-expr-proc input->when-expr)
(define [analyze-when-expr when-expr]
  (let ([predicate (analyze (when-expr-predicate when-expr))]
        [consequent (analyze (when-expr-consequent when-expr))])
    (make-when-expr predicate consequent)))
(register-tagged-expr-analyze 'when-expr analyze-when-expr)
(define [eval-when-expr analyzed-when-expr env]
  (when [is-true? (force-it (evlt (when-expr-predicate analyzed-when-expr) env))]
    (evlt (when-expr-consequent analyzed-when-expr) env)))
(register-tagged-expr-eval 'when-expr eval-when-expr)

;Unless expr
(define [make-unless-expr predicate alternative]
  (cons 'unless (cons predicate alternative)))
(define [unless-expr? tagged-expr] [tagged-expr-tag-eq? tagged-expr 'unless])
(define [unless-expr-predicte unless-expr] (car (tagged-expr-body unless-expr)))
(define [unless-expr-alternative unless-expr] (cdr (tagged-expr-body unless-expr)))
(define [input->unless-expr input]
  [and [list? input] [= (length input) 3] [eq? (car input) 'unless]
       (make-unless-expr (input->tagged-expr (cadr input))
                         (input->tagged-expr (caddr input)))])
(add-input->tagged-expr-proc input->unless-expr)
(define [analyze-unless-expr unless-expr]
  (let ([predicate (analyze (unless-expr-predicte unless-expr))]
        [alternative (analyze (unless-expr-alternative unless-expr))])
    (make-unless-expr predicate alternative)))
(register-tagged-expr-analyze 'unless analyze-unless-expr)
(define [eval-unless-expr analyzed-unless-expr env]
  (unless [is-true? (force-it (evlt (unless-expr-predicte analyzed-unless-expr) env))]
    (evlt (unless-expr-alternative analyzed-unless-expr) env)))
(register-tagged-expr-eval 'unless eval-unless-expr)

;Cond clause expr
(define [make-cond-clause-expr predicate begin-expr]
  (cons 'cond-clause-expr (cons predicate begin-expr)))
(define [cond-clause-expr? tagged-expr]
  [tagged-expr-tag-eq? tagged-expr 'cond-clause-expr])
(define [cond-clause-expr-predicate cond-clause-expr]
  (car (tagged-expr-body cond-clause-expr)))
(define [cond-clause-expr-consequent cond-clause-expr]
  (cdr (tagged-expr-body cond-clause-expr)))
(define [input->cond-clause input]
  [and [list? input] [< 1 (length input)] [not [eq? (car input) 'else]]
       (make-cond-clause-expr
         (input->tagged-expr (car input))
         (make-begin-expr (map input->tagged-expr (cdr input))))])

;Else clause expr
(define [make-else-clause-expr begin-expr]
  (cons 'else-clause-expr begin-expr))
(define [else-clause-expr? tagged-expr]
  [tagged-expr-tag-eq? tagged-expr 'else-clause-expr])
(define [else-clause-expr-consequent else-clause-expr]
  (tagged-expr-body else-clause-expr))
(define [input->else-clause input]
  [and [list? input] [< 1 (length input)] [eq? (car input) 'else]
       (make-else-clause-expr
         (make-begin-expr (map input->tagged-expr (cdr input))))])

;Cond expr
(define [make-cond-expr clauses] (cons 'cond-expr clauses))
(define [cond-expr? tagged-expr] [tagged-expr-tag-eq? tagged-expr 'cond-expr])
(define [cond-expr-clauses cond-expr] (tagged-expr-body cond-expr))
(define [input->cond-expr input]
  [and [list? input] [< 2 (length input)] [eq? (car input) 'cond]
       (make-cond-expr
         (map (lambda [x] (ormap (lambda [p] (p x))
                                 (list input->cond-clause input->else-clause)))
              (cdr input)))])
(add-input->tagged-expr-proc input->cond-expr)
(define [cond->nested-if-exprs cond-expr]
  (define [expand-clauses clauses]
    (cond ([null? clauses] (void))
          ([and [else-clause-expr? (car clauses)] [not [null? (cdr clauses)]]]
           (error "cond else clause followed by clause -- EXPAND-CLAUSES"))
          ([and [else-clause-expr? (car clauses)] [null? (cdr clauses)]]
           (else-clause-expr-consequent (car clauses)))
          (else
           (make-if-expr (cond-clause-expr-predicate (car clauses))
                         (cond-clause-expr-consequent (car clauses))
                         (expand-clauses (cdr clauses))))))
  (expand-clauses (cond-expr-clauses cond-expr)))
(define [analyze-cond-expr cond-expr]
  (let* ([nested-if-exprs (cond->nested-if-exprs cond-expr)])
    (analyze nested-if-exprs)))
(register-tagged-expr-analyze 'cond-expr analyze-cond-expr)

;Lambda expr
(define [make-lambda-expr parameters exprs]
  (cons 'lambda-expr (cons parameters exprs)))
(define [lambda-expr? tagged-expr] [tagged-expr-tag-eq? tagged-expr 'lambda-expr])
(define [lambda-expr-parameters tagged-expr] (car (tagged-expr-body tagged-expr)))
(define [lambda-expr-exprs tagged-expr] (cdr (tagged-expr-body tagged-expr)))
(define [input->lambda-expr input]
  [and [list? input] [< 2 (length input)] [eq? (car input) 'lambda]
       [or [symbol? (cadr input)]
           [and [list? (cadr input)] [andmap symbol? (cadr input)]]]
       (make-lambda-expr (cadr input) (map input->tagged-expr (cddr input)))])
(add-input->tagged-expr-proc input->lambda-expr)
(define [analyze-lambda-expr lambda-expr]
  (let ([analyzed-exprs (map analyze (lambda-expr-exprs lambda-expr))])
    (make-lambda-expr (lambda-expr-parameters lambda-expr) analyzed-exprs)))
(register-tagged-expr-analyze 'lambda-expr analyze-lambda-expr)
(define [eval-lambda-expr analyzed-lambda-expr env]
  (let ([params (lambda-expr-parameters analyzed-lambda-expr)])
    (lambda args
      (cond ([not [list? params]]
             (eval-sequence
               (lambda-expr-exprs analyzed-lambda-expr)
               (extend-environment env (list (make-vv-pair params args)))))
            ([= (length args) (length params)]
             (eval-sequence
               (lambda-expr-exprs analyzed-lambda-expr)
               (extend-environment env (map make-vv-pair params args))))
            (else
              (error "parameters and arguments inconsist -- EVAL-LAMBDA-EXPR"))))))
(register-tagged-expr-eval 'lambda-expr eval-lambda-expr)

;Definition expr
(define [make-definition-expr variable value]
  (cons 'define-expr (cons variable value)))
(define [definition-expr? tagged-expr]
  [tagged-expr-tag-eq? tagged-expr 'define-expr])
(define [definition-expr-variable definition-expr]
  (car (tagged-expr-body definition-expr)))
(define [definition-expr-expr definition-expr]
  (cdr (tagged-expr-body definition-expr)))
(define [input->definition-expr input]
  (cond ([and [list? input] [= (length input) 3] [eq? (car input) 'define]
              [symbol? (cadr input)]]
         (make-definition-expr (cadr input) (input->tagged-expr (caddr input))))
        ([and [list? input] [< 2 (length input)] [eq? (car input) 'define]
              [list? (cadr input)] [symbol? (caadr input)]]
         (make-definition-expr
           (caadr input)
           (make-lambda-expr (cdadr input)
                             (map input->tagged-expr (cddr input)))))
        (else false)))
(add-input->tagged-expr-proc input->definition-expr)
(define [analyze-definition-expr definition-expr]
  (make-definition-expr (definition-expr-variable definition-expr)
                        (analyze (definition-expr-expr definition-expr))))
(register-tagged-expr-analyze 'define-expr analyze-definition-expr)
(define [eval-definition-expr analyzed-definition-expr env]
  (define-variable!
    env
    (definition-expr-variable analyzed-definition-expr)
    (evlt (definition-expr-expr analyzed-definition-expr) env))
  'definition-ok)
(register-tagged-expr-eval 'define-expr eval-definition-expr)

;Let tag expr
(define [make-let-expr pa-pairs exprs] (cons 'let-expr (cons pa-pairs exprs)))
(define [let-expr? tagged-expr] [tagged-expr-tag-eq? tagged-expr 'let-expr])
(define [let-expr-pa-pairs let-expr] (car (tagged-expr-body let-expr)))
(define [let-expr-body let-expr] (cdr (tagged-expr-body let-expr)))
(define [input->let-expr input]
  [and [list? input] [< 2 (length input)] [eq? (car input) 'let]
       [list? (cadr input)]
       [andmap (lambda [x] [and [list? x] [= (length x) 2] [symbol? (car x)]])
               (cadr input)]
       (make-let-expr
         (map (lambda [x] (make-pa-pair (car x) (input->tagged-expr (cadr x))))
              (cadr input))
         (map input->tagged-expr (cddr input)))])
(add-input->tagged-expr-proc input->let-expr)
(define [analyze-let-expr let-expr]
  (let ([analyzed-arguments (map (lambda [x] (analyze (pa-pair-argument x)))
                                 (let-expr-pa-pairs let-expr))]
        [parameters (map pa-pair-parameter (let-expr-pa-pairs let-expr))]
        [analyzed-exprs (map analyze (let-expr-body let-expr))])
    (list (make-lambda-expr parameters analyzed-exprs)
          analyzed-arguments)))
(register-tagged-expr-analyze 'let-expr analyze-let-expr)

;Begin expr
(define [make-begin-expr exprs] (cons 'begin-expr exprs))
(define [begin-expr? tagged-expr] [tagged-expr-tag-eq? tagged-expr 'begin-expr])
(define [begin-expr-body begin-expr] (tagged-expr-body begin-expr))
(define [input->begin-expr input]
  [and [list? input] [< 1 (length input)] [eq? (car input) 'begin]
       (make-begin-expr (map input->tagged-expr (cdr input)))])
(add-input->tagged-expr-proc input->begin-expr)
(define [analyze-begin-expr begin-expr]
  (let ([analyzed-exprs (map analyze (begin-expr-body begin-expr))])
    (if [= (length analyzed-exprs) 1]
      (car analyzed-exprs)
      (make-begin-expr analyzed-exprs))))
(register-tagged-expr-analyze 'begin-expr analyze-begin-expr)
(define [eval-begin-expr analyzed-begin-expr env]
  (eval-sequence (begin-expr-body analyzed-begin-expr) env))
(register-tagged-expr-eval 'begin-expr eval-begin-expr)

;Set! expr
(define [make-assignment-expr variable new-value]
  (cons 'set!-expr (cons variable new-value)))
(define [assignment-expr? tagged-expr] [tagged-expr-tag-eq? tagged-expr 'set!-expr])
(define [assignment-expr-variable assignment-expr]
  (car (tagged-expr-body assignment-expr)))
(define [assignment-expr-expr assignment-expr]
  (cdr (tagged-expr-body assignment-expr)))
(define [input->assignment-expr input]
  [and [list? input] [= 3 (length input)] [eq? (car input) 'set!]
       [symbol? (cadr input)]
       (make-assignment-expr (cadr input) (input->tagged-expr (caddr input)))])
(add-input->tagged-expr-proc input->assignment-expr)
(define [analyze-assignment-expr assignment-expr]
  (let ([variable (assignment-expr-variable assignment-expr)]
        [analyzed-value (analyze (assignment-expr-expr assignment-expr))])
    (make-assignment-expr variable analyzed-value)))
(register-tagged-expr-analyze 'set!-expr analyze-assignment-expr)
(define [eval-assignment-expr analyzed-assignment-expr env]
  (define-variable! env
                    (assignment-expr-variable analyzed-assignment-expr)
                    (evlt (assignment-expr-expr analyzed-assignment-expr) env))
  'assignment-ok)
(register-tagged-expr-eval 'set!-expr eval-assignment-expr)

;Unbound expr
(define [make-unbound-expr symbol] (cons 'unbound!-expr symbol))
(define [unbound-expr? tagged-expr] [tagged-expr-tag-eq? tagged-expr 'unbound!-expr])
(define [unbound-expr-symbol unbound-expr] (tagged-expr-body unbound-expr))
(define [input->unbound-expr input]
  [and [list? input] [= (length input) 2] [eq? (car input) 'unbound!]
       [symbol? (cadr input)]
       (make-unbound-expr (cadr input))])
(add-input->tagged-expr-proc input->unbound-expr)
(define [analyze-unbound-expr unbound-expr] unbound-expr)
(register-tagged-expr-analyze 'unbound!-expr analyze-unbound-expr)
(define [eval-unbound-expr analyzed-unbound-expr env]
  (frame-remove! (env-head env) (unbound-expr-symbol analyzed-unbound-expr)))
(register-tagged-expr-eval 'unbound!-expr eval-unbound-expr)

;Procedure expr
(define [make-procedure-expr variable args]
  (cons 'procedure-expr (cons variable args)))
(define [procedure-expr? tagged-expr]
  [tagged-expr-tag-eq? tagged-expr 'procedure-expr])
(define [procedure-expr-proc proc-expr] (car (tagged-expr-body proc-expr)))
(define [procedure-expr-args proc-expr] (cdr (tagged-expr-body proc-expr)))
(define [input->procedure-expr input]
  [and [list? input] [< 0 (length input)]
       (make-procedure-expr (input->tagged-expr (car input))
                            (map input->tagged-expr (cdr input)))])
(add-input->tagged-expr-proc input->procedure-expr)
(define [analyze-procedure-expr proc-expr]
  (let ([analyzed-proc (analyze (procedure-expr-proc proc-expr))]
        [analyzed-args (map analyze (procedure-expr-args proc-expr))])
    (make-procedure-expr analyzed-proc analyzed-args)))
(register-tagged-expr-analyze 'procedure-expr analyze-procedure-expr)
(define [eval-procedure-expr analyzed-procedure-expr env]
  (let ([proc (force-it (evlt (procedure-expr-proc analyzed-procedure-expr) env))] [unevaled-args (procedure-expr-args analyzed-procedure-expr)])
    (if [primitive-procedure? proc]
      (apply proc (map (lambda [x] (force-it (evlt x env))) unevaled-args))
      (apply proc (map (lambda [x] (delay-it x env)) unevaled-args)))))
(register-tagged-expr-eval 'procedure-expr eval-procedure-expr)

;;Named let tag expr
;(define [make-named-let-expr self pa-pairs exprs]
;  (cons 'named-let self pa-pairs exprs))
;(define [named-let-expr? tagged-expr]
;  [tagged-expr-tag-eq? tagged-expr 'named-let])
;(define [named-let-self named-let-expr]
;  (car (tagged-expr-body named-let-expr)))
;(define [named-let-pa-pairs named-let-expr]
;  (cadr (tagged-expr-body named-let-expr)))
;(define [named-let-body named-let-expr]
;  (cddr (tagged-expr-body named-let-expr)))
;(define [analyze-named-let-expr named-let-expr]
;  (let* ([name (named-let-self named-let-expr)]
;         [inner-let-expr (make-let-expr (named-let-pa-pairs named-let-expr)
;                                        (named-let-body named-let-expr))]
;         [analyzed-let-expr (analyze inner-let-expr)])
;    (lambda [env]
;      (define-variable! env name (lambda [] (inner-let-expr env)))
;      (analyzed-let-expr env))))
;(define [eval-named-let-expr named-let-expr env]
;  ((analyze-named-let-expr named-let-expr) env))
;(register-tagged-expr-analyze 'named-let analyze-named-let-expr)
;
;;Let* tag expr
;(define [make-let*-expr pa-pairs exprs] (cons 'let* (cons pa-pairs exprs)))
;(define [let*? tagged-expr] [tagged-expr-tag-eq? tagged-expr 'let*])
;(define [let*-pa-pairs let*-expr] (car (tagged-expr-body let*-expr)))
;(define [let*-body let*-expr] (cdr (tagged-expr-body let*-expr)))
;(define [let*->nested-lets let*-expr]
;  (let ([pa-pairs (let*-pa-pairs let*-expr)]
;        [exprs (let*-body let*-expr)])
;    (if [null? pa-pairs]
;      exprs
;      (make-let-expr (cons (car pa-pairs) '())
;                     (let*->nested-lets
;                       (make-let*-expr (cdr pa-pairs) exprs))))))
;(define [analyze-let*-expr let*-expr]
;  (let ([analyzed-let*-expr (analyze (let*->nested-lets let*-expr))])
;    (lambda [env] (analyzed-let*-expr env))))
;(define [eval-let*-expr let*-expr env] ((analyze-let*-expr let*-expr) env))
;(register-tagged-expr-analyze 'let* analyze-let*-expr)
;
;;letrec tag expr
;(define [make-letrec-expr pa-pairs exprs] (cons 'letrec (cons pa-pairs exprs)))
;(define [letrec-expr? tagged-expr] [tagged-expr-tag-eq? tagged-expr 'letrec])
;(define [letrec-pa-pairs letrec-expr] (car (tagged-expr-body letrec-expr)))
;(define [letrec-body letrec-expr] (cdr (tagged-expr-body letrec-expr)))
;
;(define [letrec->let letrec-expr]
;  (define declare-variables
;    (map (lambda [var] (make-vv-pair var '*unassigned*))
;         (map pa-pair-parameter (letrec-pa-pairs letrec-expr))))
;  (define set-variables
;    (map (lambda [var val] (list 'set!-expr var val))
;         (map pa-pair-parameter (letrec-pa-pairs letrec-expr))
;         (map pa-pair-argument (letrec-pa-pairs letrec-expr))))
;
;  (make-let-expr declare-variables
;                 (append set-variables (letrec-body letrec-expr))))
;(define [analyze-letrec-expr letrec-expr]
;  (let ([analyzed-let-expr (analyze (letrec->let letrec-expr))])
;    (lambda [env] (analyzed-let-expr env))))
;(define [eval-letrec-expr letrec-expr env]
;  ((analyze-letrec-expr letrec-expr) env))
;(register-tagged-expr-analyze 'letrec analyze-letrec-expr)

;(define [scan-out-defines exprs]
;  (let ([definition-exprs (filter definition? exprs)]
;        [remaining-exprs (filter (lambda [x] [not [definition? x]]) exprs)])
;    (cons
;      (map (lambda [x] (cons x '*unassigned*))
;           (map definition-expr-variable definition-exprs))
;      (append (map (lambda [var val] (list 'set!-expr var val))
;                   (map definition-expr-variable definition-exprs)
;                   (map definition-expr-expr definition-exprs))
;              remaining-exprs))))
;(define [extract-defines->let-expr exprs]
;  (let* ([scan-result (scan-out-defines exprs)]
;         [pa-pairs (car scan-result)]
;         [body (cdr scan-result)])
;    (make-let-expr pa-pairs body)))

;;While tag expr
;(define [make-while-expr stop-expr exprs] (cons 'while (cons stop-expr exprs)))
;(define [while? tagged-expr] [tagged-expr-tag-eq? tagged-expr 'while])
;(define [while-stop-expr while-expr] (car (tagged-expr-body while-expr)))
;(define [while-body while-expr] (cdr (tagged-expr-body while-expr)))
;(define [while->tail-recur-lambda while-expr]
;  (make-lambda-expr '()
;                    (make-unless-expr (while-stop-expr while-expr)
;                                      (while-body while-expr))))
;(define [analyze-while-expr while-expr]
;  (let ([analyzed-lambda (analyze (while->tail-recur-lambda while-expr))])
;    (lambda [env] (analyzed-lambda env))))
;(define [eval-while-expr while-expr env] ((analyze-while-expr while-expr) env))
;(register-tagged-expr-analyze 'while analyze-while-expr)

;Input process
(define [inputs->tagged-exprs inputs]
  (let ([structed-exprs (map input->tagged-expr inputs)])
    (if [andmap tagged-expr? structed-exprs]
      structed-exprs
      (error "Unknown input -- INPUT->TAGGED-EXPR"))))

;Analyze process
(define [analyze tagged-expr]
  (let ([analyze-proc (get-tagged-expr-analyze (tagged-expr-tag tagged-expr))])
    (analyze-proc tagged-expr)))
(define [analyze-sequence exprs] (map analyze exprs))

;Eval process
(define [evlt analyzed-tagged-expr env]
  (let ([evlt-proc (get-tagged-expr-eval (tagged-expr-tag analyzed-tagged-expr))])
    (evlt-proc analyzed-tagged-expr env)))
(define [eval-sequence exprs env]
  (define [iter remaining]
    (let* ([first-expr (car remaining)]
           [rest-exprs (cdr remaining)]
           [first-result (evlt first-expr env)])
      (if [null? rest-exprs] first-result (iter rest-exprs))))
  (iter exprs))

;REPL, Interactive evaluator
;Primitive table bindings
(define primitive-table
  (list (make-vv-pair 'car car)
        (make-vv-pair 'cdr cdr)
        (make-vv-pair 'cons cons)
        (make-vv-pair 'null? null?)
        (make-vv-pair 'eq? eq?)
        (make-vv-pair 'equal? equal?)
        (make-vv-pair '+ +)
        (make-vv-pair '- -)
        (make-vv-pair '* *)
        (make-vv-pair '/ /)
        (make-vv-pair '= =)
        (make-vv-pair 'display display)
        (make-vv-pair 'newline newline)))
(define primitive-names (map vv-pair-variable primitive-table))
(define primitive-procs (map vv-pair-value primitive-table))
(define [primitive-procedure? proc] [memq proc primitive-procs])

(define [setup-environment]
  (let ([init-env (extend-environment the-empty-environment primitive-table)])
    (define-variable! init-env 'true true)
    (define-variable! init-env 'false false)
    (define-variable! init-env 'void void)
    init-env))
(define the-global-environment (setup-environment))

(define input-prompt ";;; L-Eval input:")
(define output-prompt ";;; L-Eval value:")
(define [prompt-for-input str] (newline) (newline) (displayln str))
(define [announce-output str] (newline) (displayln str))
(define [user-print object] (displayln object))
(define [driver-loop]
  (prompt-for-input input-prompt)
  (let ([input (read)])
    (let ([output (force-it (evlt (analyze (input->tagged-expr input))
                                  the-global-environment))])
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))
(driver-loop)
