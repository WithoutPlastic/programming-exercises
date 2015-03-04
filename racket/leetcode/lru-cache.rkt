#lang racket

;Problem:
;Design and implement a data structure for Least Recently Used (LRU) cache. It
;should support the following operations: get and set.
;
;get(key)
;Get the value (will always be positive) of the key if the key exists in the
;cache, otherwise return -1.
;
;set(key, value)
;Set or insert the value if the key is not already present. When the cache
;reached its capacity, it should invalidate the least recently used item before
;inserting a new item.

(define lru-cache (new (class object%
  (super-new)
  (define cache '())
  (define capacity 4)
  (define 32bit-max 2147483646)
  (define [make-kv key val {counter 1}] (list key val counter))
  (define kv-key car) (define kv-val cadr) (define kv-counter caddr)
  (define [kv-update kv
          #:key {keyf identity} #:val {valf identity} #:cnt {cntf identity}]
    (let ([key (kv-key kv)] [val (kv-val kv)] [cnt (kv-counter kv)])
      (make-kv (keyf key) (valf val) (cntf cnt))))
  (define [other-kvs key] (filter-not (compose (curry eq? key) kv-key) cache))
  (define [get-kv key] (assoc key cache eq?))

  (define [counter-reduce!]
    (let* ([reduce (λ [n] (add1 (floor (/ n 2))))]
           [reduced-cache (map (curry kv-update #:cnt reduce) cache)])
      (set! cache reduced-cache)))

  (define [remove-less-accessed-kv!]
    (set! cache (other-kvs (kv-key (argmin kv-counter cache)))))

  (define [counter-add1! key]
    (when [= (kv-counter (get-kv key)) 32bit-max] (counter-reduce!))
    (set! cache (cons (kv-update (get-kv key) #:cnt add1) (other-kvs key))))

  (define/public [cache-get key]
    (let ([kv (get-kv key)])
      (if [not kv] -1 (begin (counter-add1! key) (kv-val kv)))))

  (define/public [cache-set! key val]
    (let ([kv (get-kv key)])
      (if [not kv]
        (begin
          (when [= (length cache) capacity] (remove-less-accessed-kv!))
          (set! cache (cons (make-kv key val) cache)))
        (begin
          (set! cache (cons (kv-update kv #:val (const val)) (other-kvs key)))
          (counter-add1! key))))))))

(send lru-cache cache-set! 'a 49)
(send lru-cache cache-set! 'b 49)
(send lru-cache cache-set! 'c 49)
(send lru-cache cache-set! 'd 49)
(send lru-cache cache-get 'a)
(send lru-cache cache-get 'a)
(send lru-cache cache-get 'b)
(send lru-cache cache-get 'b)
(send lru-cache cache-get 'a)
(send lru-cache cache-get 'c)
(send lru-cache cache-set! 'e 0)
(send lru-cache cache-get 'c)
(send lru-cache cache-get 'd)
(send lru-cache cache-get 'e)
