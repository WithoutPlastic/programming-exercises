#lang racket

(define [make-table key-equal?]
  (define local-table (mcons '*table* (mcons '() '())))
  (define [find-key-value key table]
    (cond ([or (null? table) (null? (mcar table))] #f)
          ([key-equal? key (mcar (mcar table))] (mcar table))
          (else (find-key-value key (mcdr table)))))
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
               (set-mcdr! table
                          (mcons (mcons key value)
                                 (mcdr table))))))
          (else
            (let* ([key (car keys)]
                   [subtable (find-key-value key (mcdr table))])
              (if subtable
                (apply insert! subtable value (cdr keys))
                (begin
                  (set-mcdr! table
                             (mcons (mcons key '())
                                    (mcdr table)))
                  (apply insert! table value keys))))))
    'ok)
  (lambda [op . args]
    (cond ([eq? op 'lookup] (apply lookup local-table args))
          ([eq? op 'insert!] (apply insert! local-table args))
          (else (error "unknown operation -- TABLE" op)))))

(define tb (make-table eq?))
(tb 'lookup 'letters 'a)
(tb 'insert! 97 'letters 'a)
(tb 'insert! 98 'letters 'b)
(tb 'insert! 99 'letters 'c)
(tb 'insert! 100 'letters 'd)
(tb 'insert! 101 'letters 'e)
(tb 'insert! 10086 'letters 'china 'cmcc)
(tb 'insert! 43 'math '+)
(tb 'insert! 45 'math '-)
(tb 'insert! 42 'math '*)
(tb 'lookup 'letters 'a)
(tb 'lookup 'letters 'd)
(tb 'lookup 'math '+)
(tb 'lookup 'math '*)
(tb 'lookup 'letters 'china 'cmcc)
