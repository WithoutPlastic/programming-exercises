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
;(define [prime? n] (fast-prime? n 100))

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
(define [delay proc] (memo-proc proc))
(define [cons-stream stream-elt delayed-stream]
  (cons stream-elt (delay delayed-stream)))
(define [the-empty-stream] '())
(define [empty-stream? stream]
  [or [null? stream]
      [eq? stream 'stm-for-each-done]
      [eq? stream 'stm-map-done]
      [eq? stream 'stm-filter-done]])
(define [stream-car stream] (car stream))
(define [stream-cdr stream] (force (cdr stream)))
(define [stream-ref stream index]
  (if [= index 0]
    (stream-car stream)
    (stream-ref (stream-cdr stream) (sub1 index))))
(define [stream-for-each proc stream]
  (if [empty-stream? stream]
    'stm-for-each-done
    (begin
      (proc (stream-car stream))
      (stream-for-each proc (stream-cdr stream)))))
(define [stream-map proc stream]
  (if [empty-stream? stream]
    'stm-map-done
    (cons-stream (proc (stream-car stream))
                       (lambda [] (stream-map proc (stream-cdr stream))))))
(define [stream-filter pred stream]
  (cond ([empty-stream? stream] 'stm-filter-done)
        ([pred (stream-car stream)]
         (cons-stream (stream-car stream)
                      (lambda [] (stream-filter pred (stream-cdr stream)))))
        (else (stream-filter pred (stream-cdr stream)))))

(define [display-line item] (newline) (display item))
(define [display-stream stream]
  (stream-for-each display-line stream))
(define [show x] (display-line x) x)

(define [stream-enumerate-interval from to]
  (if [< from to]
    (cons-stream from
                 (lambda [] (stream-enumerate-interval (add1 from) to)))
    (list to)))

(define [integers-starting-from n]
  (cons-stream n (lambda [] (integers-starting-from (add1 n)))))
;(define integers (integers-starting-from 1))
(define [divisible? x divisor] [= (remainder x divisor) 0])
;
;(define no-sevens
;  (stream-filter (lambda [x] [not [divisible? x 7]])
;                 integers))
;
;(define [fibgen a b] (cons-stream a (lambda [] (fibgen b (+ a b)))))
;(define fibs (fibgen 0 1))
;
;(define [sieve stream]
;  (cons-stream
;    (stream-car stream)
;    (lambda [] (sieve (stream-filter
;                   (lambda [x] [not [divisible? x (stream-car stream)]])
;                   (stream-cdr stream))))))
;(define primes (sieve (integers-starting-from 2)))
;

;Implicitly define streams
(define ones (cons-stream 1 (lambda [] ones)))
(define [alt-stream-map proc . stream-args]
  (if [ormap empty-stream? stream-args]
    the-empty-stream
    (cons-stream
      (apply proc (map stream-car stream-args))
      (lambda [] (apply alt-stream-map proc (map stream-cdr stream-args))))))

(define [add-streams stream-a stream-b] (alt-stream-map + stream-a stream-b))
(define [mul-streams stream-a stream-b] (alt-stream-map * stream-a stream-b))

(define integers (cons-stream 1 (lambda [] (add-streams ones integers))))

(define fibs
  (cons-stream
    0
    (lambda []
      (cons-stream
        1
        (lambda [] (add-streams (stream-cdr fibs)
                                fibs))))))

(define [scale-stream stream factor]
  (stream-map (lambda [x] (* x factor)) stream))

(define double
  (cons-stream 1 (lambda [] (scale-stream double 2))))

(define [prime? n]
  (define [iter prime-stream]
    (cond ([< n (square (stream-car prime-stream))] #t)
          ([divisible? n (stream-car prime-stream)] #f)
          (else (iter (stream-cdr prime-stream)))))
  (iter primes))

(define primes
  (cons-stream 2
               (lambda [] (stream-filter prime? (integers-starting-from 3)))))

(define factorials
  (cons-stream 1
               (lambda [] (mul-streams (integers-starting-from 1)
                                       factorials))))

(define [partial-sums stream]
  (cons-stream (stream-car stream)
               (lambda [] (add-streams (partial-sums stream)
                                       (stream-cdr stream)))))
