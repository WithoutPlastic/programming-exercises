#lang racket

(interpret '(define [remove-item item items]
              (cond ([null? items] null)
                    ([equal? item (car items)] (cdr items))
                    (else (cons (car items))
                          (remove-item item (cdr items))))))
(interpret '(define [random-element-of items]
              (require [not [null? items]])
              (let ([item (list-ref items (random (length items)))])
                (amb item (random-element-of (remove-item item items))))))
(interpret '(define [parse-word word-list]
              (require [not [null? *unparsed*]])
              (let ([found-word (random-element-of (cdr word-list))])
                (set! *unparsed* (cdr *unparsed*))
                (list (car word-list) found-word))))
