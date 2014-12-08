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

(define ones (cons-stream 1 (lambda [] ones)))
(define zeros (cons-stream 0 (lambda [] zeros)))
(define integers (cons-stream 1 (lambda [] (add-streams ones integers))))
(define [integers-start-from n]
  (cons-stream n (lambda [] (integers-start-from (add1 n)))))
(define [integrate-series series] (alt-stream-map / series integers))
(define [derivate-series series]
  (alt-stream-map * (stream-cdr series) integers))
(define exp-series (cons-stream 1 (lambda [] (integrate-series exp-series))))
(define cosine-series
  (cons-stream
    1
    (lambda []
      (neg-series
        (integrate-series
          (cons-stream 0;expand sine-series again here
                       (lambda [] (integrate-series cosine-series))))))))
(define sine-series
  (cons-stream 0 (lambda [] (integrate-series cosine-series))))
(define [mul-series series-a series-b]
  (cons-stream (* (stream-car series-a)
                  (stream-car series-b))
               (lambda []
                 (integrate-series
                   (add-streams (mul-series series-a
                                             (derivate-series series-b))
                                (mul-series series-b
                                             (derivate-series series-a)))))))
(define one
  (add-streams (mul-series cosine-series cosine-series)
               (mul-series sine-series sine-series)))
(define [coone-series series]
  (cons-stream 1
               (lambda []
                 (neg-series (mul-series (stream-cdr series))
                             coone-series))))
(define [div-series numer-series denom-series]
  (let ([denom-const-term (stream-car denom-series)])
    (if [= (stream-car denom-series) 0]
      (error "denominator series contains a zero constant part -- DIV-SERIES")
      (scale-stream
        (mul-series numer-series
                    (scale-stream denom-series
                                  (/ 1 (stream-car denom-series))))
        (stream-car denom-series)))))
(define tangent-series (div-series sine-series cosine-series))

(define [sqrt-improve guess x] (average guess (/ x guess)))
(define [sqrt-stream x]
  (define guesses
    (cons-stream 1.0
                 (lambda []
                   (stream-map (lambda [guess] (sqrt-improve guess x))
                               guesses))))
  guesses)

(define [pi-summands n]
  (cons-stream (/ 1.0 n) (lambda [] (stream-map - (pi-summands (+ n 2))))))
(define pi-stream (scale-stream (partial-sums (pi-summands 1)) 4))

(define [euler-transform stream]
  (let ([s0 (stream-ref stream 0)]
        [s1 (stream-ref stream 1)]
        [s2 (stream-ref stream 2)])
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (lambda [] (euler-transform (stream-cdr stream))))))
(define [make-tableau transform stream]
  (cons-stream stream
               (lambda [] (make-tableau transform (transform stream)))))

(define [accelerated-sequence transform stream]
  (stream-map stream-car (make-tableau transform stream)))
(define [stream-limit stream tolerance]
  (let ([current-term (stream-car stream)]
        [next-term (stream-car (stream-cdr stream))])
    (if [< (abs (- current-term next-term)) tolerance]
      next-term
      (stream-limit (stream-cdr stream) tolerance))))
(define [sqrt x tolerance]
  (stream-limit [sqrt-stream x] tolerance))

;(stream-filter (lambda [pair]
;                 (prime? (+ (car pair) (cadr pair))))
;               (in-pairs))

;(stream-map (lambda [x] (list (stream-car stream-s) x)) (stream-cdr stream-t))
(define [interleave stream-a stream-b]
  (cond ([empty-stream? stream-a] stream-b)
        ([empty-stream? stream-b] stream-a)
        (else (cons-stream (stream-car stream-a)
                           (lambda [] (interleave stream-b
                                                  (stream-cdr stream-a)))))))
(define [triple-interleave stream-s stream-t stream-v]
  (cond ([empty-stream? stream-s] (interleave stream-t stream-v))
        ([empty-stream? stream-t] (interleave stream-s stream-v))
        ([empty-stream? stream-v] (interleave stream-s stream-t))
        (else (cons-stream (stream-car stream-s)
                           (lambda []
                             (triple-interleave stream-t
                                                stream-v
                                                (stream-cdr stream-s)))))))

(define [alt-pairs stream-s stream-t]
  (cons-stream
    (list (stream-car stream-s) (stream-car stream-t))
    (lambda []
      (triple-interleave
        (stream-map (lambda [x] (list (stream-car stream-s) x))
                    (stream-cdr stream-t))
        (stream-map (lambda [x] (list (stream-car stream-t) x))
                    (stream-cdr stream-s))
        (pairs (stream-cdr stream-s) (stream-cdr stream-t))))))
(define [pairs stream-s stream-t]
  (cons-stream
    (list (stream-car stream-s) (stream-car stream-t))
    (lambda []
      (interleave
        (stream-map (lambda [x] (list (stream-car stream-s) x))
                    (stream-cdr stream-t))
        (pairs (stream-cdr stream-s) (stream-cdr stream-t))))))

(define [triples stream-s stream-t stream-u]
  (let ([cur-i (stream-car stream-s)]
        [cur-j (stream-car stream-t)]
        [cur-k (stream-car stream-u)])
    (cons-stream
      (list cur-i cur-j cur-k)
      (lambda []
        (interleave
          (stream-map (lambda [x] (list cur-i (car x) (cadr x)))
                      (pairs stream-t (stream-cdr stream-u)))
          (triples (stream-cdr stream-s)
                   (stream-cdr stream-t)
                   (stream-cdr stream-u)))))))
(define pythagoras
  (stream-filter
    (lambda [triple] [= (+ (square (car triple))
                           (square (cadr triple)))
                        (square (caddr triple))])
    (triples integers integers integers)))

(define [weighted-merge stream-a stream-b weight]
  (define [inner-merge]
    (let ([first-elt-a (stream-car stream-a)]
          [first-elt-b (stream-car stream-b)])
      (cond ([< (weight first-elt-a) (weight first-elt-b)]
             (cons-stream first-elt-a
                          (lambda []
                            (weighted-merge (stream-cdr stream-a)
                                            stream-b
                                            weight))))
            ([< (weight first-elt-b) (weight first-elt-a)]
             (cons-stream first-elt-b
                          (lambda []
                            (weighted-merge stream-a
                                            (stream-cdr stream-b)
                                            weight))))
            (else
             (cons-stream first-elt-a
                          (lambda []
                            (weighted-merge (stream-cdr stream-a)
                                            stream-b
                                            weight)))))))
  (cond ([empty-stream? stream-a] stream-b)
        ([empty-stream? stream-b] stream-a)
        (else (inner-merge))))

(define [weighted-pairs stream-s stream-t weight]
  (cons-stream
    (list (stream-car stream-s) (stream-car stream-t))
    (lambda []
      (weighted-merge
        (stream-map (lambda [x] (list (stream-car stream-s) x))
                    (stream-cdr stream-t))
        (weighted-pairs (stream-cdr stream-s) (stream-cdr stream-t) weight)
        weight))))

(define [cube-sum-weight pair] (+ (cube (car pair)) (cube (cadr pair))))
(define [square-sum-weight pair] (+ (square (car pair)) (square (cadr pair))))
(define [same-weight-filter stream weight]
  (let ([first-elt (stream-car stream)]
        [second-elt (stream-car (stream-cdr stream))])
    (if [= (weight first-elt) (weight second-elt)]
      (cons-stream first-elt
                   (lambda []
                     (same-weight-filter (stream-cdr stream)
                                         weight)))
      (same-weight-filter (stream-cdr stream) weight))))

(define ramanujans
  (same-weight-filter
    (weighted-pairs integers integers cube-sum-weight)
    cube-sum-weight))
(define ramanujans-with-weight
  (stream-map (lambda [pair] (cons pair (cube-sum-weight pair)))
              ramanujans))

(define [triple-weight-repeat-filter stream weight]
  (define [repeat-shift cur-stream]
    (let* ([first-elt (stream-car cur-stream)]
           [remaining-stream (stream-cdr cur-stream)]
           [second-elt (stream-car remaining-stream)])
      (if [= (weight first-elt) (weight second-elt)]
        (cons-stream first-elt
                     (lambda [] (repeat-shift remaining-stream)))
        (cons-stream first-elt
                     (lambda [] (non-repeat-shift remaining-stream))))))
  (define [non-repeat-shift cur-stream]
    (let* ([first-elt (stream-car cur-stream)]
           [remaining-stream (stream-cdr cur-stream)]
           [second-elt (stream-car remaining-stream)]
           [third-elt (stream-car (stream-cdr remaining-stream))])
      (if [= (weight first-elt)
             (weight second-elt)
             (weight third-elt)]
        (cons-stream first-elt
                     (lambda [] (repeat-shift remaining-stream)))
        (non-repeat-shift remaining-stream))))

  (non-repeat-shift stream))
(define triple-repeat-square-sum-weight-pairs
  (stream-map (lambda [pair] (cons pair (square-sum-weight pair)))
              (triple-weight-repeat-filter
                (weighted-pairs integers
                                integers
                                square-sum-weight)
                square-sum-weight)))

;(define [integral integrand initial-value dt]
;  (define inner-integral
;    (cons-stream initial-value
;                 (lambda [] (add-streams (scale-stream integrand dt)
;                                         inner-integral))))
;  inner-integral)
;
;(define [RC-circuit R C dt]
;  (lambda [i v0]
;    (add-streams (scale-stream i R)
;                 (integral (scale-stream i (/ 1 C)) v0 dt))))
;(define one-RC (RC-circuit 5 1 0.5))

;(define [solve f y0 dt]
;  (define y (integral dy y0 dt))
;  (define dy (stream-map f y))
;  y)
;
;normal integral with delay, implicitly defined
;(define [integral delayed-integrand initial-value dt]
;  (define inner-integral
;    (cons-stream initial-value
;                 (lambda []
;                   (let ([integrand (force delayed-integrand)])
;                     (add-streams (scale-stream integrand dt)
;                                  inner-integral)))
;                 ))
;  inner-integral)

(define [integral delayed-integrand initial-value dt]
  (cons-stream initial-value
               (lambda []
                 (let ([integrand (force delayed-integrand)])
                   (if [empty-stream? integrand]
                     the-empty-stream
                     (integral (lambda [] (stream-cdr integrand))
                               (+ (* dt (stream-car integrand))
                                  initial-value)
                               dt))))))
(define [solve f y0 dt]
  (define y (integral (lambda [] dy) y0 dt))
  (define dy (stream-map f y))
  y)

(define [solve-2nd a b dt y0 dy0]
  (define y (integral (lambda [] dy) y0 dt))
  (define dy (integral (lambda [] ddy) dy0 dt))
  (define ddy (add-streams (scale-stream dy a)
                           (scale-stream y b)))
  y)

(display-stream (solve-2nd 2 2 0.001 1 1))

