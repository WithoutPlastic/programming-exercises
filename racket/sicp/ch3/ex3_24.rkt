#lang racket

(define [make-table key-equal?]
  (define local-table (mcons '*table* (mcons '() '())))
  (define [find-key-value key table]
    (cond ([or (null? table) (null? (mcar table))] #f)
          ([key-equal? key (mcar (mcar table))] (mcar table))
          (else (find-key-value key (mcdr table)))))
  (define [lookup key-1 key-2]
    (let ([subtable (find-key-value key-1 (mcdr local-table))])
      (if subtable
        (let ([record (find-key-value key-2 (mcdr subtable))])
          (if record
            (mcdr record)
            #f))
        #f)))
  (define [insert! key-1 key-2 value]
    (let ([subtable (find-key-value key-1 (mcdr local-table))])
      (if subtable
        (let ([record (find-key-value key-2 (mcdr subtable))])
          (if record
            (set-mcdr! record value)
            (set-mcdr! subtable
                       (mcons (mcons key-2 value)
                              (mcdr subtable)))))
        (set-mcdr! local-table
                   (mcons (mcons key-1
                                 (mcons (mcons key-2 value) '()))
                          (mcdr local-table)))))
    'ok)
  (lambda [op . args]
    (cond ([eq? op 'lookup] (apply lookup args))
          ([eq? op 'insert!] (apply insert! args))
          (else (error "unknown operation -- TABLE" op)))))

(define tb (make-table eq?))
(tb 'lookup 'letters 'a)
(tb 'insert! 'letters 'a 97)
(tb 'insert! 'letters 'b 98)
(tb 'insert! 'letters 'c 99)
(tb 'insert! 'letters 'd 100)
(tb 'insert! 'letters 'e 101)
(tb 'insert! 'math '+ 43)
(tb 'insert! 'math '- 45)
(tb 'insert! 'math '* 42)
(tb 'lookup 'letters 'a)
(tb 'lookup 'letters 'd)
(tb 'lookup 'math '+)
(tb 'lookup 'math '*)
