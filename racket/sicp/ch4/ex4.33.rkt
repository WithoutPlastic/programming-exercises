#lang racket

(define [eval-quote-expr analyzed-quote-expr env]
  (let ([text (quote-expr-text analyzed-quote-expr)])
    (if [pair? text]
      (eval (list 'cons (list 'quote (car text)) (list 'quote (cdr text))) env)
      text)))

;Answer: We need to repack tagged expr when eval quote expr, so that further
;eval will refer the newly defined 'cons to construct new data structure.
