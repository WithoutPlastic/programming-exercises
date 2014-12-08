#lang racket

(define [average num-a num-b] (/ (+ num-a num-b) 2))
(define [square x] (* x x))
(define [cube x] (* x x x))

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
(define [display-stream stream] (stream-for-each display-line stream))
(define [show x] (display-line x) x)

(define [alt-stream-map proc . stream-args]
  (if [ormap empty-stream? stream-args]
    the-empty-stream
    (cons-stream
      (apply proc (map stream-car stream-args))
      (lambda [] (apply alt-stream-map proc (map stream-cdr stream-args))))))
(define [scale-stream stream factor]
  (stream-map (lambda [x] (* x factor)) stream))

(define [add-streams stream-a stream-b] (alt-stream-map + stream-a stream-b))
(define [sub-streams stream-a stream-b] (alt-stream-map - stream-a stream-b))
(define [mul-streams stream-a stream-b] (alt-stream-map * stream-a stream-b))
(define [div-streams stream-a stream-b] (alt-stream-map / stream-a stream-b))
(define [neg-series series] (stream-map (lambda [x] (- x)) series))
(define [partial-sums stream]
  (cons-stream (stream-car stream)
               (lambda [] (add-streams (stream-cdr stream)
                                       (partial-sums stream)))))

(define [gen-random] (random 2147483647))
(define random-init 10086)
(define rand
  (begin
    (random-seed random-init)
    (lambda [seed]
      (random-seed seed)
      (gen-random))))

(define random-numbers
  (cons-stream random-init
               (lambda [] (stream-map rand random-numbers))))

(define [convert-random stream]
  (define [iter shadow elt]
    (cond ([eq? elt 'generate] (rand shadow))
          ([number? elt] (rand elt))
          (else (error "unknown stream format -- CONVERT-RANDOM"))))
  (cons-stream random-init
               (lambda []
                 (alt-stream-map iter
                                 (convert-random stream)
                                 stream))))

(define [map-successive-pairs f stream]
  (cons-stream (f (stream-car stream) (stream-car (stream-cdr stream)))
               (lambda []
                 (map-successive-pairs f (stream-cdr (stream-cdr stream))))))
(define cesaro-stream
  (map-successive-pairs
    (lambda [random-a random-b] [= (gcd random-a random-b) 1])
    random-numbers))

(define [monte-carlo experiment-stream]
  (define [iter remaining-experiment-stream cur-pass-cnt cur-fail-cnt]
    (cons-stream (/ cur-pass-cnt (+ cur-pass-cnt cur-fail-cnt))
                 (lambda [] 
                   (if [stream-car remaining-experiment-stream]
                     (iter (stream-cdr remaining-experiment-stream)
                           (add1 cur-pass-cnt)
                           cur-fail-cnt)
                     (iter (stream-cdr remaining-experiment-stream)
                           cur-pass-cnt
                           (add1 cur-fail-cnt))))))
  (iter experiment-stream 1 1))

(define [random-pairs-between-one-gen]
  (cons-stream (cons (random) (random))
               (lambda [] (random-pairs-between-one-gen))))
(define random-pairs-between-one (random-pairs-between-one-gen))
;(display-stream random-pairs-between-one)
(define pi
  (scale-stream
    (monte-carlo
      (stream-map
        (lambda [pair]
          [< (+ (square (- (car pair) 0.5))
                    (square (- (cdr pair) 0.5)))
                 0.25])
        random-pairs-between-one))
    4.0))

(display-stream pi)
