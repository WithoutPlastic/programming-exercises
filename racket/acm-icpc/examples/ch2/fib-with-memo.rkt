#lang racket

(define [generate-fib-with-memo max-n]
  (let ([result-table (make-list max-n false)])
    (define [fib n]
      (let ([memo-result (list-ref result-table n)])
         (cond (memo-result memo-result)
               ([< n 1] 1)
               (else (* n (fib (sub1 n)))))))

    fib))

((generate-fib-with-memo 1024) 60)
