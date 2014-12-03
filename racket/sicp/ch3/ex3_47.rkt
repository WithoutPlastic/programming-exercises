#lang racket

(define [test-and-set! cell]
  (if [car cell]
    #t
    (begin (set-car! cell #t) #f)))
(define [clear! cell] (set-car! cell #f))

(define [make-mutex]
  (let ([cell (list #f)])
    (define [the-mutex msg]
      (cond ([eq? msg 'acquire]
             (when [test-and-set! cell]
               (the-mutex 'acquire)))
            ([eq? msg 'release]
             (clear! cell))))
    the-mutex))

(define [make-semaphore max-acquire]
  (let ([inner-mutex (make-mutex)]
        [acquire-counter 0])
    (define semaphore [msg]
      (cond ([eq? msg 'acquire]
             (inner-mutex 'acquire)
             (if [< acquire-counter max-acquire]
               (begin
                 (set! acquire-counter (add1 acquire-counter))
                 (inner-mutex 'release))
               (begin
                 (inner-mutex 'release)
                 (semaphore 'acquire))))
            ([eq? msg 'release]
             (inner-mutex 'acquire)
             (if [< 0 acquire-counter]
               (begin
                 (set! acquire-counter (sub1 acquire-counter))
                 (inner-mutex 'release))
               (begin
                 (inner-mutex 'release)
                 (error "over release semaphore detected -- SEMAPHORE"))))
            (else (error "unknown request -- MAKE-SEMAPHORE" msg))))
    semaphore))

;The only thing to do, is using mutex on inner counter, to make sure counter
;result is always accurate in parallel cases
