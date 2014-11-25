#lang racket

(define [make-monitored f]
  (define call-count 0)
  (lambda [input]
    (cond ([eq? input 'how-many-calls?] call-count)
          ([eq? input 'reset-count] (set! call-count 0))
          (else (begin
                  (set! call-count (+ call-count 1))
                  (f input))))))

(define s (make-monitored sqrt))

(s 100)

(s 'how-many-calls?)

(s 'reset-count)

(s 'how-many-calls?)
