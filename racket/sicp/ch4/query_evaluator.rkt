#lang racket

(require racket/promise)

;Steam manipulation
(define [the-empty-stream] '())
(define [stream-cons elt stream] (cons elt (delay stream)))
(define [stream-car stream] (car stream))
(define [stream-cdr stream] (force (cdr stream)))
(define [stream? object]
  [or [eq? object the-empty-stream]
      [and [pair? object]
           [promise? (cadr object)]]])
(define [stream-empty? stream] [eq? stream the-empty-stream])
(define stream-null? stream-empty?)
(define [stream-map proc stream]
  (if [stream-empty? stream]
    the-empty-stream
    (stream-cons (proc (stream-car stream))
                 (stream-map proc (stream-cdr stream)))))
(define [singleton-stream x] (stream-cons x the-empty-stream))
(define [singleton-stream? x]
  [and [not [stream-empty? x]] [stream-empty? (stream-cdr x)]])
(define [stream-foreach proc stream]
  (unless [stream-empty? stream]
    (proc (stream-car stream))
    (stream-foreach proc (stream-cdr stream))))
(define [stream-filter pred stream]
  (cond ([stream-empty? stream] the-empty-stream)
        ([pred (stream-car stream)]
         (stream-cons (stream-car stream)
                      (stream-filter pred (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))
(define [stream-append stream-a delayed-stream-b]
  (if [stream-empty? stream-a]
    (force delayed-stream-b)
    (stream-cons (stream-car stream-a)
                 (stream-append (stream-cdr stream-a) delayed-stream-b))))
(define [stream-interleave stream-a delayed-stream-b]
  (if [stream-empty? stream-a]
    (force delayed-stream-b)
    (stream-cons (stream-car stream-a)
                 (stream-interleave (force delayed-stream-b)
                                    (delay (stream-cdr stream-a))))))
(define [stream-flattern stream]
  (if [stream-empty? stream]
    the-empty-stream
    (stream-interleave (stream-car stream)
                       (stream-flattern (stream-cdr stream)))))
(define [stream-flatmap proc stream] (stream-map proc (stream-flattern stream)))
(define [stream-mapflat proc stream] (stream-flattern (stream-map proc stream)))

;Binding structure
(define [make-binding var val] (cons var val))
(define [binding-variable binding] (car binding))
(define [binding-value binding] (cdr binding))
(define the-empty-binding '())

;Bindings structure
(define [make-empty-bindings] (mcons 'bindings the-empty-stream))
(define [bindings-header bindings] (mcar bindings))
(define [bindings-body bindings] (mcdr bindings))
(define [bindings? object]
  [and [mpair? object]
       [eq? (bindings-header object) 'bindings]
       [stream? (bindings-body object)]])
(define [empty-bindings? bindings]
  [and [bindings? bindings] [stream-empty? (bindings-body bindings)]])
(define [add-new-bindings bindings vars vals]
  (define [iter remaining-vars remaining-vals]
    (if [or [null? remaining-vars] [null? remaining-vals]]
      (bindings-body bindings)
      (stream-cons (make-binding (car remaining-vars) (car remaining-vals))
                   (iter (cdr remaining-vars) (cdr remaining-vals)))))
  (if [= (length vars) (length vals)]
    (mcons 'bindings (iter vars vals))
    (error "binding variables to values length inconsist -- ADD-NEW-BINDINGS")))
(define [make-bindings vars vals]
  (add-new-bindings (make-empty-bindings) vars vals))
(define [extend-bindings bindings new-vars new-vals]
  (add-new-bindings bindings new-vars new-vals))
(define [get-binding-by-variable variable bindings]
  (define [iter remaining]
    (cond ([stream-empty? remaining] the-empty-stream)
          ([equal? (binding-variable (stream-car remaining)) variable]
           (stream-car remaining))
          (else (iter (stream-cdr remaining)))))
  (iter (bindings-body bindings)))

;Failed bindings
(define [bindings->failed-bindings bindings]
  (mcons 'failed-bindings (bindings-body bindings)))
(define [failed-bindings? object]
  [and [mpair? object]
       [eq? (bindings-header object) 'failed-bindings]
       [list? (bindings-body object)]])

;Tools
(define [lndisplay x] (newline) (display x))
(define [display-stream stream] (stream-foreach lndisplay stream))

(define [symbol-w/-?-prefix? x]
  [and [symbol? x] [string=? (substring (symbol->string x) 0 1) "?"]])
(define [symbol-w/o-?-prefix? x]
  [and [symbol? x] [not [string=? (substring (symbol->string x) 0 1) "?"]]])
(define [constant? x] [or [symbol-w/o-?-prefix? x] [number? x]])
(define [symbol-or-number? x] [or [symbol? x] [number? x]])

;Tree map
(define [tree-map tree proc]
  (define [walk elt]
    (cond ([and [list? elt] [null? elt]] '())
          ([pair? elt] (cons (walk (car elt)) (walk (cdr elt))))
          (else (proc elt))))
  (walk tree))

(define [constant-tree->tagged-expr-tree tree]
  [tree-map tree constant-input->tagged-expr])
(define [query-symbol-tree->tagged-expr-tree tree]
  [tree-map tree input->query-symbol-expr])
(define [number-symbol-tree->tagged-expr-tree tree]
  [tree-map tree number-symbol-input->tagged-expr])

;Tree scan
(define [and2 x y] [and x y])
(define [or2 x y] [or x y])
(define [tree-scan tree elt-check? and-or-merge]
  (define [walk elt dflt-v]
    (cond ([and [list? elt] [null? elt]] dflt-v)
          ([pair? elt] [and-or-merge (walk (car elt)) (walk (cdr elt))])
          (else [elt-check? elt])))
  (cond ([eq? and-or-merge and2] (walk tree true))
        ([eq? and-or-merge or2] (walk tree false))
        (else (error "unknown merge procedure -- TREE-SCAN" and-or-merge))))

(define [constant-tree? tree]
  [tree-scan tree constant? and2])
(define [number-symbol-tree-with-query? tree]
  [tree-scan tree symbol-or-number? and2])
(define [query-symbol-tree? tree]
  [tree-scan tree symbol-w/-?-prefix? and2])

;Tagged structure
(define [tagged-expr? object] [and [list? object] [symbol? (car object)]])
(define [tagged-expr-tag tagged-expr] (car tagged-expr))
(define [tagged-expr-body tagged-expr] (cdr tagged-expr))
(define [tagged-expr-tag-eq? tagged-expr tag]
  [eq? (tagged-expr-tag tagged-expr) tag])

;Input->tagged-expr table
(define input->tagged-expr-list '())
(define [add-input->tagged-expr-proc proc]
  (set! input->tagged-expr-list (cons proc input->tagged-expr-list)))
(define [input->tagged-expr input]
  [ormap (lambda [x] (x input))
         input->tagged-expr-list])
;         (append (filter (lambda [x] [or [not [eq? x input->assert-expr]]
;                                         [not [eq? x input->rule-expr]]])
;                         input->tagged-expr-list)
;                 (filter (lambda [x] [eq? x input->assert-expr])
;                         input->tagged-expr-list)
;                 (filter (lambda [x] [eq? x input->rule-expr])
;                         input->tagged-expr-list))])
(define [constant-input->tagged-expr input]
  [ormap (lambda [x] (x input))
         (list input->number-expr input->constant-symbol-expr)])
(define [symbol-input->tagged-expr input]
  [ormap (lambda [x] (x input))
         (list input->query-symbol-expr
               input->constant-symbol-expr)])
(define [number-symbol-input->tagged-expr input]
  [ormap (lambda [x] (x input))
         (list input->number-expr
               input->constant-symbol-expr
               input->query-symbol-expr)])

;Pattern expr
(define [input-pattern? input]
  [or [input-assert-pattern? input]
      [input-rule-pattern? input]
      [input-conjunction? input]
      [input-disjunction? input]
      [input-negate? input]
      [input-lisp-value? input]])
(define [pattern-input->tagged-expr input]
  [ormap (lambda [x] (x input))
         (list input->assert-pattern-expr
               input->rule-pattern-expr
               input->conjunction-expr
               input->disjunction-expr
               input->negate-expr
               input->lisp-value-expr)])

;Tagged-expr analyze table
(define tagged-expr-analyze-table (make-hash))
(define [register-tagged-expr-analyze tag analyze-proc]
  (hash-set! tagged-expr-analyze-table tag analyze-proc))
(define [get-tagged-expr-analyze tag]
  (hash-ref tagged-expr-analyze-table tag))

;Tagged-expr qeval table
(define tagged-expr-qeval-table (make-hash))
(define [register-tagged-expr-qeval tag qeval-proc]
  (hash-set! tagged-expr-qeval-table tag qeval-proc))
(define [get-tagged-expr-qeval tag]
  (hash-ref! tagged-expr-qeval-table tag))

;Constant assert database
(define all-assertions the-empty-stream)
(define [get-all-assert-exprs] all-assertions)
(define indexed-assert-expr-table (make-hash))
(define [store-indexed-assert-exprs index-key datum]
  (hash-set! indexed-assert-expr-table index-key datum))
(define [get-indexed-assert-exprs index-key]
  (hash-ref indexed-assert-expr-table index-key the-empty-stream))

(define [fetch-assert-exprs domain key value]
  (if [constant-symbol-expr? domain]
    (get-indexed-assert-exprs (constant-symbol-expr-symbol domain))
    (get-all-assert-exprs)))
(define [add-assert-expr! domain key value]
  (let ([index-key (constant-symbol-expr-symbol domain)]
        [datum (cons domain (cons key value))]
        [old-all-assertions all-assertions])
    (store-indexed-assert-exprs 
      index-key
      (stream-cons datum (get-indexed-assert-exprs index-key)))
    (set! all-assertions (stream-cons datum old-all-assertions)))
  'add-assert-ok)

;Rule database
(define all-rules the-empty-stream)
(define [get-all-rule-exprs] all-rules)
(define indexed-rule-expr-table (make-hash))
(define [store-indexed-rule-exprs index-key datum]
  (hash-set! indexed-rule-expr-table index-key datum))
(define [get-indexed-rule-exprs index-key]
  (hash-ref indexed-rule-expr-table index-key the-empty-stream))

(define [fetch-rule-exprs name conclusion details]
  (if [constant-symbol-expr? name]
    (get-indexed-rule-exprs (constant-symbol-expr-symbol name))
    (get-all-rule-exprs)))
(define [add-rule-expr! name conclusion details]
  (let ([index-key (constant-symbol-expr-symbol name)]
        [datum (cons name (cons conclusion details))]
        [old-all-rules all-rules])
    (store-indexed-rule-exprs
      index-key
      (stream-cons datum (get-indexed-rule-exprs index-key)))
    (set! all-rules (stream-cons datum old-all-rules)))
  'add-rule-ok)

;Number expr
(define [make-number-expr number] (cons 'number-expr number))
(define [number-expr? tagged-expr] [tagged-expr-tag-eq? tagged-expr 'number-expr])
(define number-expr-number tagged-expr-body)
(define input-number? number?)
(define [input->number-expr input]
  [and [input-number? input] (make-number-expr input)])
(add-input->tagged-expr-proc input->number-expr)
(register-tagged-expr-analyze 'number-expr (lambda [x] x))

;Constant symbol expr
(define [make-constant-symbol-expr symbol] (cons 'constant-symbol-expr symbol))
(define [constant-symbol-expr? tagged-expr]
  [tagged-expr-tag-eq? tagged-expr 'constant-symbol-expr])
(define constant-symbol-expr-symbol tagged-expr-body)
(define input-constant-symbol? symbol-w/o-?-prefix?)
(define [input->constant-symbol-expr input]
  [and [input-constant-symbol? input] (make-constant-symbol-expr input)])
(add-input->tagged-expr-proc input->constant-symbol-expr)
(register-tagged-expr-analyze 'constant-symbol-expr (lambda [x] x))

;Query symbol expr
(define [make-query-symbol-expr symbol] (cons 'query-symbol-expr symbol))
(define [query-symbol-expr? tagged-expr]
  [tagged-expr-tag-eq? tagged-expr 'query-symbol-expr])
(define query-symbol-expr-symbol tagged-expr-body)
(define input-query-symbol? symbol-w/-?-prefix?)
(define [strip-first-? symbol-w/-?-prefix]
  (let ([str (symbol->string symbol-w/-?-prefix)])
    (string->symbol (substring str 1 (string-length str)))))
(define [input->query-symbol-expr input]
  [and [input-query-symbol? input] 
       (make-query-symbol-expr (strip-first-? input))])
(add-input->tagged-expr-proc input->query-symbol-expr)
(register-tagged-expr-analyze 'query-symbol-expr (lambda [x] x))

;Add assert expr
(define [make-add-assert!-expr domain key value]
  (cons 'add-assert!-expr (cons domain (cons key value))))
(define [add-assert!-expr? tagged-expr]
  [tagged-expr-tag-eq? tagged-expr 'add-assert!-expr])
(define [add-assert!-expr-domain add-assert!-expr]
  (car (tagged-expr-body add-assert!-expr)))
(define [add-assert!-expr-key add-assert!-expr]
  (cadr (tagged-expr-body add-assert!-expr)))
(define [add-assert!-expr-value add-assert!-expr]
  (cddr (tagged-expr-body add-assert!-expr)))
(define [input-add-assert? input]
  [and [list? input] [= 3 (length input)]
       [eq? (car input) 'add-assert!]
       [input-constant-symbol? (cadr input)]
       [list? (caddr input)] [= 2 (length (caddr input))]
       [constant-tree? (caddr input)]])
(define [input->add-assert!-expr input]
  [and [input-add-assert? input]
       (make-add-assert!-expr
         (input->constant-symbol-expr (cadr input))
         (constant-tree->tagged-expr-tree (caddr input)))])
(add-input->tagged-expr-proc input->add-assert!-expr)
(register-tagged-expr-analyze 'add-assert!-expr (lambda [x] x))
(define [qeval-add-assert!-expr analyzed-add-assert!-expr _]
  (add-assert-expr!
    (add-assert!-expr-domain analyzed-add-assert!-expr)
    (add-assert!-expr-key analyzed-add-assert!-expr)
    (add-assert!-expr-value analyzed-add-assert!-expr)))
(register-tagged-expr-qeval 'add-assert!-expr qeval-add-assert!-expr)

;Add rule expr
(define [make-add-rule!-expr name conclusion details]
  (cons 'add-rule!-expr (cons name (cons conclusion details))))
(define [add-rule!-expr tagged-expr]
  [tagged-expr-tag-eq? tagged-expr 'add-rule!-expr])
(define [add-rule!-expr-name add-rule!-expr]
  (car (tagged-expr-body add-rule!-expr)))
(define [add-rule!-expr-conclusion add-rule!-expr]
  (cadr (tagged-expr-body add-rule!-expr)))
(define [add-rule!-expr-details add-rule!-expr]
  (cddr (tagged-expr-body add-rule!-expr)))
(define [input-add-rule? input]
  [and [list? input] [= 4 (length input)]
       [eq? (car input) 'add-rule!]
       [input-constant-symbol? (cadr input)]
       [list? (caddr input)] [not [null? (caddr input)]]
       [query-symbol-tree? (caddr input)]
       [input-pattern? (cadddr input)]])
(define [input->add-rule!-expr input]
  [and [input-add-rule? input]
       (make-add-rule!-expr
         (input->constant-symbol-expr (cadr input))
         (query-symbol-tree->tagged-expr-tree (caddr input))
         (number-symbol-tree->tagged-expr-tree (cadddr input)))])
(add-input->tagged-expr-proc input->add-rule!-expr)
(register-tagged-expr-analyze 'add-rule!-expr (lambda [x] x))
(define [qeval-add-rule!-expr analyzed-add-rule!-expr _]
  (add-rule-expr!
    (add-rule!-expr-name analyzed-add-rule!-expr)
    (add-rule!-expr-conclusion analyzed-add-rule!-expr)
    (add-rule!-expr-details analyzed-add-rule!-expr)))
(register-tagged-expr-qeval 'add-rule!-expr qeval-add-rule!-expr)

;Pattern match
(define [pattern-match pattern datum bindings]
  (define [lookup-binding-and-try-again]
    (let ([binding (get-binding-by-variable pattern)])
      (if [null? binding]
        (extend-bindings bindings pattern datum)
        (pattern-match (binding-value binding) datum bindings))))

  (cond ([failed-bindings? bindings] bindings)
        ([equal? datum pattern] bindings)
        ([query-symbol-expr? pattern] (lookup-binding-and-try-again))
        ([and [pair? datum] [pair? pattern]]
         (pattern-match (cdr datum)
                        (cdr pattern)
                        (pattern-match (car datum)
                                       (car pattern)
                                       bindings)))
        (else (bindings->failed-bindings bindings))))

;Assert pattern expr
(define [make-assert-pattern-expr domain key value]
  (cons 'assert-pattern-expr (cons domain (cons key value))))
(define [assert-pattern-expr? tagged-expr]
  [tagged-expr-tag-eq? tagged-expr 'assert-pattern-expr])
(define [assert-pattern-expr-domain assert-pattern-expr]
  (car (tagged-expr-body assert-pattern-expr)))
(define [assert-pattern-expr-key assert-pattern-expr]
  (cadr (tagged-expr-body assert-pattern-expr)))
(define [assert-pattern-expr-value assert-pattern-expr]
  (cddr (tagged-expr-body assert-pattern-expr)))
(define assert-pattern-expr-pattern tagged-expr-body)
(define [input-assert-pattern? input]
  [and [list? input] [= 3 (length input)]
       [eq? (car input) 'assert]
       [symbol? (cadr input)]
       [list? (caddr input)] [= 2 (length (caddr input))]
       [number-symbol-tree-with-query? (cadr input)]])
(define [input->assert-pattern-expr input]
  [and [input-assert-pattern? input]
       (make-assert-pattern-expr
         (symbol-input->tagged-expr (cadr input))
         (number-symbol-tree->tagged-expr-tree (caddr input)))])
(add-input->tagged-expr-proc input->assert-pattern-expr)
(register-tagged-expr-analyze 'assert-pattern-expr (lambda [x] x))
(define [apply-assert-pattern-on-assertions assert-pattern-expr bindings]
  (define [validate-pattern assertion]
    (let* ([pattern (assert-pattern-expr-pattern assert-pattern-expr)]
           [result (pattern-match pattern assertion bindings)])
      (if result (singleton-stream result) the-empty-stream)))

  (let* ([dom (assert-pattern-expr-domain assert-pattern-expr)]
         [assertions (fetch-assert-exprs dom empty empty)])
    (stream-mapflat validate-pattern assertions)))
(define [qeval-assert-pattern-expr analyzed-assert-pattern-expr bindings-stream]
  (stream-mapflat
    (lambda [bindings]
      (apply-assert-pattern-on-assertions analyzed-assert-pattern-expr bindings))
    bindings-stream))
(register-tagged-expr-qeval 'assert-pattern-expr qeval-assert-pattern-expr)

;Rule counter, rule query symbol anti-alias
(define rule-analyze-counter 0)
(define [rename-symbol symbol cur-cnt]
  (string->symbol
    (string-append (symbol->string symbol) "-" (number->string cur-cnt))))
(define [rename-query-symbol tagged-expr cur-cnt]
  (if [query-symbol-expr? tagged-expr]
    (make-query-symbol-expr
      (rename-symbol (query-symbol-expr-symbol tagged-expr) cur-cnt))
    tagged-expr))
(define [tree->renamed-query-symbol-tree tree cur-cnt]
  (tree-map tree (lambda [x] (rename-query-symbol x cur-cnt))))
(define [decorate-rule-counter]
  (let ([new-counter (add1 rule-analyze-counter)])
    (set! rule-analyze-counter new-counter)
    new-counter))

;Unify match
(define [unify-match pattern-a pattern-b bindings]
  (define [depends-on? var pattern]
    (tree-scan pattern
               (lambda [x]
                 [and [query-symbol-expr? x]
                      [or [equal? var x]
                          (let ([binding (get-binding-by-variable x)])
                            (if [pair? binding]
                              (depends-on? var (binding-value binding))
                              false))]])
               or2))

  (define [lookup-binding-and-try-again pattern-a pattern-b]
    (cond ([and [not [query-symbol-expr? pattern-a]]
                [query-symbol-expr? pattern-b]]
           (lookup-binding-and-try-again pattern-b pattern-a))
          ([and [query-symbol-expr? pattern-a]
                [not [query-symbol-expr? pattern-b]]]
           (let* ([var-b (query-symbol-expr-symbol pattern-b)]
                  [binding-b (get-binding-by-variable var-b)])
             (cond ([and [null? binding-b]
                         [depends-on? pattern-b pattern-a]]
                    (bindings->failed-bindings bindings))
                   ([and [null? binding-b]
                         [not [depends-on? pattern-b pattern-a]]]
                    (extend-bindings bindings pattern-a pattern-b))
                   (else (unify-match pattern-a
                                      (binding-value pattern-b)
                                      bindings)))))
          (else (let* ([var-a (query-symbol-expr-symbol pattern-a)]
                       [var-b (query-symbol-expr-symbol pattern-b)]
                       [binding-a (get-binding-by-variable var-a)]
                       [binding-b (get-binding-by-variable var-b)])
                  (cond ([not [null? binding-a]]
                         (unify-match (binding-value binding-a)
                                      pattern-b
                                      bindings))
                        ([not [null? binding-b]]
                         (unify-match pattern-a
                                      (binding-value binding-b)
                                      bindings))
                        (else (extend-bindings bindings
                                               pattern-a
                                               pattern-b)))))))

  (cond ([failed-bindings? bindings] bindings)
        ([equal? pattern-a pattern-b] bindings)
        ([or [query-symbol-expr? pattern-a] [query-symbol-expr? pattern-b]]
         (lookup-binding-and-try-again pattern-a pattern-b))
        ([and [pair? pattern-a] [pair? pattern-b]]
         (unify-match (cdr pattern-a)
                      (cdr pattern-b)
                      (unify-match (car pattern-a)
                                   (car pattern-b)
                                   bindings)))
        (else (bindings->failed-bindings bindings))))

;Rule pattern expr
(define [make-rule-pattern-expr name conclusion details]
  (cons 'rule-pattern-expr (cons name (cons conclusion details))))
(define [rule-pattern-expr? tagged-expr]
  [tagged-expr-tag-eq? tagged-expr 'rule-pattern-expr])
(define [rule-pattern-expr-name rule-pattern-expr]
  (car (tagged-expr-body rule-pattern-expr)))
(define [rule-pattern-expr-conclusion rule-pattern-expr]
  (cadr (tagged-expr-body rule-pattern-expr)))
(define [rule-pattern-expr-details rule-pattern-expr]
  (cddr (tagged-expr-body rule-pattern-expr)))
(define rule-pattern-expr-pattern tagged-expr-body)
(define [input-rule-pattern? input]
  [and [list? input] [= 3 (length input)]
       [eq? (car input) 'rule]
       [symbol? (cadr input)]
       [list? (caddr input)] [< 1 (length (caddr input))]
       [number-symbol-tree-with-query? (caddr input)]])
(define [input->rule-pattern-expr input]
  [and [input-rule-pattern? input]
       (make-rule-pattern-expr
         (symbol-input->tagged-expr (cadr input))
         (number-symbol-tree->tagged-expr-tree (caddr input)))])
(add-input->tagged-expr-proc input->rule-pattern-expr)
(register-tagged-expr-analyze 'rule-pattern-expr (lambda [x] x))
(define [apply-rule-pattern-on-assertions rule-pattern-expr bindings]
  (define [validate-pattern rule]
    (let* ([cur-cnt (decorate-rule-counter)]
           [aliased-rule (tree->renamed-query-symbol-tree rule cur-cnt)]
           [conclusion (cadr aliased-rule)]
           [details (cddr aliased-rule)]
           [pattern (rule-pattern-expr-pattern rule-pattern-expr)]
           [unified-ret (unify-match pattern conclusion bindings)])
      (if [failed-bindings? unified-ret]
        the-empty-stream
        (qeval details (singleton-stream unified-ret)))))
  (let* ([name (rule-pattern-expr-name rule-pattern-expr)]
         [rules (fetch-rule-exprs name empty empty)])
    (stream-mapflat rules)))
(define [qeval-rule-pattern-expr analyzed-rule-pattern-expr bindings-stream]
  (stream-mapflat
    (lambda [bindings]
      (apply-rule-pattern-on-assertions analyzed-rule-pattern-expr bindings))
    bindings-stream))
(register-tagged-expr-qeval 'rule-pattern-expr qeval-rule-pattern-expr)

;Conjuction pattern expr
(define [make-conjunction-expr patterns]
  (cons 'conjunction-expr patterns))
(define [conjunction-expr? tagged-expr]
  [tagged-expr-tag-eq? tagged-expr 'conjunction-expr])
(define conjunction-expr-patterns tagged-expr-body)
(define [input-conjunction? input]
  [and [list? input] [< 2 (length input)]
       [eq? (car input) 'and]
       [andmap input-pattern? (cdr input)]])
(define [input->conjunction-expr input]
  [and [input-conjunction? input]
       (make-conjunction-expr (map pattern-input->tagged-expr (cdr input)))])
(add-input->tagged-expr-proc input->conjunction-expr)
(register-tagged-expr-analyze 'conjunction-expr (lambda [x] x))
(define [qeval-conjunction-pattern-expr analyzed-conjunction-pattern-expr
                                        bindings-stream]
  (define [iter remaining-patterns bindings-stream]
    (if [null? remaining-patterns]
      bindings-stream
      (iter (cdr remaining-patterns)
            (qeval (car remaining-patterns) bindings-stream))))
  (iter (conjunction-expr-patterns analyzed-conjunction-pattern-expr)
        bindings-stream))
(register-tagged-expr-qeval 'conjunction-expr qeval-conjunction-pattern-expr)

;Disjunction pattern expr
(define [make-disjunction-expr patterns]
  (cons 'disjunction-expr patterns))
(define [disjunction-expr? tagged-expr]
  [tagged-expr-tag-eq? tagged-expr 'disjunction-expr])
(define disjunction-expr-patterns tagged-expr-body)
(define [input-disjunction? input]
  [and [list? input] [< 2 (length input)]
       [eq? (car input) 'or]
       [andmap input-pattern? (cdr input)]])
(define [input->disjunction-expr input]
  [and [input-disjunction? input]
       (make-disjunction-expr (map pattern-input->tagged-expr (cdr input)))])
(add-input->tagged-expr-proc input->disjunction-expr)
(register-tagged-expr-analyze 'disjunction-expr (lambda [x] x))
(define [qeval-disjunction-pattern-expr analyzed-disjunction-pattern-expr
                                        bindings-stream]
  (define [iter remaining-patterns bindings-stream]
    (if [null? remaining-patterns]
      the-empty-stream
      (stream-interleave
        (qeval (car remaining-patterns) bindings-stream)
        (delay (iter (cdr remaining-patterns) bindings-stream)))))
  (iter (disjunction-expr-patterns analyzed-disjunction-pattern-expr)
        bindings-stream))
(register-tagged-expr-qeval 'disjunction-expr qeval-disjunction-pattern-expr)

;Negate pattern expr
(define [make-negate-expr pattern] (cons 'negate-expr pattern))
(define [negate-expr? tagged-expr]
  [tagged-expr-tag-eq? tagged-expr 'negate-expr])
(define negate-expr-pattern tagged-expr-body)
(define [input-negate? input]
  [and [list? input] [= 2 (length input)]
       [eq? (car input) 'not]
       [input-pattern? (cadr input)]])
(define [input->negate-expr input]
  [and [input-negate? input]
       (make-negate-expr (pattern-input->tagged-expr (cadr input)))])
(add-input->tagged-expr-proc input->negate-expr)
(register-tagged-expr-analyze 'negate-expr (lambda [x] x))
(define [qeval-negate-expr analyzed-negate-expr bindings-stream]
  (stream-mapflat
    (lambda [bindings]
      (let ([result (qeval (negate-expr-pattern analyzed-negate-expr))])
        (if [stream-null? result]
          (singleton-stream bindings)
          the-empty-stream)))
    bindings-stream))
(register-tagged-expr-qeval 'negate-expr qeval-negate-expr)

;Unique pattern expr
(define [make-unique-expr pattern] (cons 'unique-expr pattern))
(define [unique-expr? tagged-expr]
  [tagged-expr-tag-eq? tagged-expr 'unique-expr])
(define unique-expr-pattern tagged-expr-body)
(define [input-unique? input]
  [and [list? input] [= 2 (length input)]
       [eq? (car input) 'unique]
       [input-pattern? (cadr input)]])
(define [input->unique-expr input]
  [and [input-unique? input]
       [make-unique-expr (pattern-input->tagged-expr (cadr input))]])
(add-input->tagged-expr-proc input->unique-expr)
(register-tagged-expr-analyze 'unique-expr (lambda [x] x))
(define [qeval-unique-expr analyzed-unique-expr bindings-stream]
  (stream-mapflat
    (lambda [bindings]
      (let ([result (qeval (unique-expr-pattern analyzed-unique-expr))])
        (if [singleton-stream? result] result the-empty-stream)))
    bindings-stream))
(register-tagged-expr-qeval 'unique-expr qeval-unique-expr)

;Lisp-value pattern expr
(define [make-lisp-value-expr verdict] (cons 'lisp-value-expr verdict))
(define [lisp-value-expr? tagged-expr]
  [tagged-expr-tag-eq? tagged-expr 'lisp-value-expr])
(define lisp-value-expr-verdict tagged-expr-body)
(define [input-lisp-value? input]
  [and [list? input] [= 2 (length input)]
       [eq? (car input) 'lisp-value]
       [pair? (cadr input)]
       [andmap symbol-or-number? (cadr input)]
       [not [number? input]]])
(define [input->lisp-value-expr input]
  [and [input-lisp-value? input]
       (make-lisp-value-expr (cadr input))])
(add-input->tagged-expr-proc input->lisp-value-expr)
(register-tagged-expr-analyze 'lisp-value-expr (lambda [x] x))
(define [qeval-lisp-value-expr analyzed-lisp-value-expr bindings-stream]
  (define [invalid-val-handler value bindings]
    (error "unknown verdict value -- QEVAL-LISP-VALUE-EXPR" value))
  (stream-mapflat
    (lambda [bindings]
      (let* ([verdict (lisp-value-expr-verdict analyzed-lisp-value-expr)]
             [binded-verdict (bind verdict bindings invalid-val-handler)]
             [result (execute binded-verdict)])
        (if result (singleton-stream bindings) the-empty-stream)))
    bindings-stream))
(register-tagged-expr-qeval 'lisp-value-expr qeval-lisp-value-expr)

;Always-true pattern expr
(define [make-always-true-expr] (cons 'always-true-expr '()))
(define [always-true-expr? tagged-expr]
  [tagged-expr-tag-eq? tagged-expr 'always-true-expr])
(define [input-always-true? input]
  [and [list? input] [= 1 (length input)]
       [eq? (car input) 'always-true]])
(define [input->always-true-expr input]
  [and [input-always-true? input] (make-always-true-expr)])
(add-input->tagged-expr-proc input->always-true-expr)
(register-tagged-expr-analyze 'always-true-expr (lambda [x] x))
(define [qeval-always-true-expr _ bindings-stream] bindings-stream)
(register-tagged-expr-qeval 'always-true-expr qeval-always-true-expr)

;Constructe user initial environment
(define [generate-user-initial-environment] (make-base-namespace))
(define user-initial-environment (generate-user-initial-environment))

;Execute expr in user lisp environment
(define [execute expr]
  (apply (eval (car expr) user-initial-environment) (cdr expr)))

;Analyze
(define [analyze tagged-expr]
  (let ([analyze-proc (get-tagged-expr-analyze tagged-expr)])
    (analyze-proc tagged-expr)))
;Qeval
(define [qeval analyzed-query bindings-stream]
  (let ([qeval-proc (get-tagged-expr-qeval analyzed-query)])
    (qeval-proc analyzed-query bindings-stream)))

;Bind
(define [bind expr bindings unbound-val-handler]
  (define [walk remaining]
    (cond ([query-symbol-expr? remaining]
           (let* ([var (query-symbol-expr-symbol remaining)]
                  [binding (get-binding-by-variable var bindings)])
             (if [pair? binding]
               (walk (binding-value binding))
               (unbound-val-handler expr bindings))))
          ([pair? expr] (cons (walk (car expr)) (walk (cdr expr))))
          (else expr)))
  (walk expr))

;Driver loop
(define [prompt-for-input prompt]  (newline) (displayln prompt))
(define [prompt-for-output prompt] (displayln prompt))
(define input-prompt ";;; Query input: ")
(define output-prompt ";;; Query output: ")

(define [query-driver-loop]
  (prompt-for-input input-prompt)
  (let* ([input-query (read)]
         [tagged-expr (input->tagged-expr input-query)]
         [analyzed-expr (analyze tagged-expr)]
         [result (qeval analyzed-expr (singleton-stream '()))])
    (prompt-for-output output-prompt)
    (if [symbol? result]
      (displayln result)
      (display-stream
        (stream-map
          (lambda [bindings]
            (bind input-query
                  bindings
                  (lambda [val bindings]
                    (error "aliased value -- DRIVER-LOOP" val))))
          result)))
    (query-driver-loop)))

(query-driver-loop)
