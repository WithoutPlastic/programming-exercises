#lang racket

(define [make-element key value] (mcons key value))
(define [element-key element] (mcar element))
(define [element-value element] (mcdr element))
(define [element-key<? element-a key]
  (bytes<? (string->bytes/utf-8 (symbol->string (element-key element-a)))
           (string->bytes/utf-8 (symbol->string key))))
(define [element-key=? element key]
  (eq? (element-key element) key))

(define [make-btree] (mcons '() (mcons '() '())))
(define [make-leaf element] (mcons element (mcons '() '())))
(define [empty-btree? btree]
  (and (null? (mcar btree))
       (null? (mcar (mcdr btree)))
       (null? (mcdr (mcdr btree)))))
(define [left-branch node] (mcar (mcdr node)))
(define [right-branch node] (mcdr (mcdr node)))
(define [node-element node] (mcar node))
(define [set-left-branch! node next-node] (set-mcar! (mcdr node) next-node))
(define [set-right-branch! node next-node] (set-mcdr! (mcdr node) next-node))
(define [set-element! node element] (set-mcar! node element))
(define [leaf-node? node]
  (and (not (null? (node-element node)))
       (null? (right-branch node))
       (null? (left-branch node))))

(define [node-insert! node element]
  (cond ([empty-btree? node] (set-mcar! node element))
        ([and (element-key<? element (element-key (node-element node)))
              (null? (left-branch node))]
         (set-left-branch! node (make-leaf element)))
        ([and (element-key<? element (element-key (node-element node)))
              (not (null? (left-branch node)))]
         (node-insert! (left-branch node) element))
        ([and (element-key<? (node-element node) (element-key element))
              (null? (right-branch node))]
         (set-right-branch! node (make-leaf element)))
        ([and (element-key<? (node-element node) (element-key element))
              (not (null? (right-branch node)))]
         (node-insert! (right-branch node) element))
        ([element-key=? (node-element node) (element-key element)]
         (error "insert element key already existed -- NODE-INSERT!" element))
        (else (error "insert element failed -- NODE-INSERT!" element))))
(define [search-node-with-key node key]
  (cond ([or (empty-btree? node)
             (and (leaf-node? node)
                  (not (element-key=? (node-element node) key)))] #f)
        ([element-key=? (node-element node) key] (node-element node))
        ([and (element-key<? (node-element node) key)
              (null? (right-branch node))] #f)
        ([and (element-key<? (node-element node) key)
              (not (null? (right-branch node)))]
         (search-node-with-key (right-branch node) key))
        ([and (not (element-key<? (node-element node) key))
              (null? (left-branch node))] #f)
        (else
          (search-node-with-key (left-branch node) key))))

(define [make-table key-equal?]
  (define local-table (mcons '*table* (make-btree)))
  (define [find-key-value key node] (search-node-with-key node key))
  (define [lookup table . keys]
    (cond ([null? keys] #f)
          ([null? (cdr keys)]
           (let* ([key (car keys)]
                  [record (find-key-value key (mcdr table))])
             (if record
               (mcdr record)
               #f)))
          (else
            (let* ([key (car keys)]
                   [subtable (find-key-value key (mcdr table))])
              (if subtable
                (apply lookup subtable (cdr keys))
                #f)))))
  (define [insert! table value . keys]
    (cond ([null? keys] #f)
          ([null? (cdr keys)]
           (let* ([key (car keys)]
                  [record (find-key-value key (mcdr table))])
             (if record
               (set-mcdr! record value)
               (node-insert! (mcdr table) (make-element key value)))))
          (else
            (let* ([key (car keys)]
                   [subtable (find-key-value key (mcdr table))])
              (if subtable
                (apply insert! subtable value (cdr keys))
                (begin
                  (node-insert! (mcdr table) (make-element key (make-btree)))
                  (apply insert! table value keys))))))
    'ok)
  (lambda [op . args]
    (cond ([eq? op 'lookup] (apply lookup local-table args))
          ([eq? op 'insert!] (apply insert! local-table args))
          (else (error "unknown operation -- TABLE" op)))))

(define [fib n]
  (cond ([= n 0] 0)
        ([= n 1] 1)
        (else
          (+ (fib (- n 1))
             (fib (- n 2))))))

(define [memoize f]
  (let ([table (make-table eq?)])
    (lambda [x]
      (let ([previous-computed-result (table 'lookup x)])
        (or previous-computed-result
            (let ([result (f x)])
              (table 'insert! (make-element x result))
              result))))))

(define memo-fib
  (memoize (lambda [n]
             (cond ([= n 0] 0)
                   ([= n 1] 1)
                   (else (+ (memo-fib (- n 1))
                            (memo-fib (- n 2))))))))

(memo-fib 10)
