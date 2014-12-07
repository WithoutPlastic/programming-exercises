#lang racket

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
;(define [neg-series series] (scale-stream series -1))

(define ones (cons-stream 1 (lambda [] ones)))
(define zeros (cons-stream 0 (lambda [] zeros)))
(define integers (cons-stream 1 (lambda [] (add-streams ones integers))))
(define [integers-start-from n]
  (cons-stream n (lambda [] (integers-start-from (add1 n)))))
;(define [integrate-series series]
;  (mul-streams series (sub-streams ones integers)))
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

;common-solution
;(define [mul-series series-a series-b]
;  (cons-stream (* (stream-car series-a) (stream-car series-b))
;               (lambda []
;                 (add-streams
;                   (add-streams (scale-stream (stream-cdr series-a)
;                                              (stream-car series-b))
;                                (scale-stream (stream-cdr series-b)
;                                              (stream-car series-a)))
;                   (cons-stream 0
;                                (lambda []
;                                  (mul-series (stream-cdr series-a)
;                                              (stream-cdr series-b))))))))

;alternative solution
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
    ;The coone-series need constant part to be one 
    ;(mul-series numer-series
    ;            (coone-series denom-series)))))
  
(define tangent-series (div-series sine-series cosine-series))
