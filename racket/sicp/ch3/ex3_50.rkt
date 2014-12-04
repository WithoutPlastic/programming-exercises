#lang racket

(define [square x] (* x x))
(define [expmod base exp m]
  (cond ([= exp 0] 1)
        ([even? exp] (remainder (square (expmod base (/ exp 2) m)) m))
        (else (remainder (* base (expmod base (- exp 1) m)) m))))
(define [fermat-test n]
  (define [iter x] [= (expmod x n n) x])
  (iter (+ 1 (random (- n 1)))))
(define [fast-prime? n times]
  (cond ([= times 0] #t)
        ([fermat-test n] [fast-prime? n (- times 1)])
        (else #f)))
(define [prime? n] (fast-prime? n 100))

(define [memo-proc proc]
  (let ([already-run? #f] [result #f])
    (lambda []
      (unless already-run?
        (set! result (proc))
        (set! already-run? #t))
      result)))

(define [force delay-proc]
  (if [null? delay-proc]
    '()
    (delay-proc)))
(define [delay proc . args] (memo-proc (lambda [] (apply proc args))))
(define [cons-stream stream-elt delayed-stream]
  (cons stream-elt delayed-stream))
(define [the-empty-stream] '())
(define [stream-null? stream] [null? stream])
(define [stream-car stream] (car stream))
(define [stream-cdr stream] (force (cdr stream)))
(define [stream-ref stream index]
  (if [= index 0]
    (stream-car stream)
    (stream-ref (stream-cdr stream) (sub1 index))))
(define [stream-for-each proc stream]
  (if [stream-null? stream]
    'stm-for-each-done
    (begin
      (proc (stream-car stream))
      (stream-for-each proc (stream-cdr stream)))))
(define [stream-map proc stream]
  (if [stream-null? stream]
    'stm-map-done
    (cons-stream (proc (stream-car stream)
                       (delay stream-map proc (stream-cdr stream))))))
(define [stream-filter pred stream]
  (cond ([stream-null? stream] 'stm-filter-done)
        ([pred (stream-car stream)]
         (cons-stream (stream-car stream)
                      (delay stream-filter pred (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))
(define [display-stream stream]
  (define [display-line item] (newline) (display item))
  (stream-for-each display-line stream))
(define [stream-enumerate-interval from to]
  (if [< from to]
    (cons-stream from
                 (delay stream-enumerate-interval (add1 from) to))
    (list to)))

(stream-car
  (stream-cdr
    (stream-filter prime?
                   (stream-enumerate-interval 10000 1000000))))
