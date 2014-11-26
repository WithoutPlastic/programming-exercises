#lang racket

(define [contain-cycle? x]
  (define [iter y visited]
    (cond ([not (pair? y)] #f)
          ([memq y visited] #t)
          (else (iter (cdr y (cons y visited))))))
  (iter x))
