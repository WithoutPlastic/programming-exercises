#lang racket


(define [memorize-func func]
  (let ([hash-table (make-hash)])
    (λ args
       (let ([key args])
         (if [hash-has-key? hash-table key] (hash-ref hash-table key)
           (let ([result (apply func args)])
             (hash-set! hash-table key result)
             result))))))

(provide (all-defined-out))
