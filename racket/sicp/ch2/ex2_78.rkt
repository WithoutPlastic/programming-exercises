#lang racket

(define [attach-tag type-tag contents]
  (cond ([number? contents] contents)
        (else (cons type-tag contents))))

(define [type-tag typed-data]
  (cond ([number? typed-data] 'scheme-number)
        ([pair? typed-data] (car typed-data))
        (else (error "Bad tagged typed data -- TYPE-TAG" typed-data))))

(define [contents typed-data]
  (cond ([number? typed-data] typed-data)
        ([pair? typed-data] (cdr typed-data))
        (else (error "Bad tagged typed data -- CONTENTS" typed-data))))
