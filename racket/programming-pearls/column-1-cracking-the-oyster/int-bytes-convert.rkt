#lang racket

(define [int->4bytes int]
  (define [iter remaining r]
    (if [< r 0] '()
      (let* ([w (expt 256 r)]
             [cur-byte (floor (/ remaining w))]
             [next (- remaining (* w cur-byte))])
        (cons cur-byte (iter next (sub1 r))))))

  (list->bytes (iter int 3)))

(define [4bytes->int bstr]
  (foldl + 0 (map (λ [i e] (* i (expt 256 e))) (bytes->list bstr) (range 0 4))))

(provide (all-defined-out))
