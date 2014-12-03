#lang racket

(define [make-serializer f] (lambda [] (f)))
(define [parallel-execute . fs]
  (if [null? fs]
    'done
    (begin
      (car fs)
      (parallel-execute (cdr fs)))))

(define [make-account balance]
  (define [withdraw amount]
    (if [<= amount balance]
      (begin
        (set! balance (- balance amount))
        balance)
      "Insufficient funds"))
  (define [deposit amount]
    (set! balance (+ balance amount))
    balance)
  (let ([protected (make-serializer)])
    (define [dispatch op . args]
      (cond ([eq? op 'withdraw] (apply (protected withdraw) args))
            ([eq? op 'deposit] (apply (protected deposit) args))
            ([eq? op 'balance] (balance))
            (else (error "unknown operation -- MAKE-ACCOUNT" op))))
    dispatch))

(define x 10)
(parallel-execute (lambda [] (set! x (* x x)))
                  (lambda [] (set! x (* x x x))))

;Parallel execution cases
;
;atomic procedure square, apply-square-changes, cube, apply-cube-changes
;apply-square-changes always after square
;apply-cube-changes always after cube
;
;Time elapse ----------------------------------------------------------------->
;case 0
;initial        square  apply-square-changes    cube    apply-cube-changes
; 10            10(100) 100                     100(1000000)    1000000
;
;case 1
;initial        square  cube    apply-square-changes    apply-cube-changes
; 10            10(100) 10(100,1000) 100(1000)          1000
;
;case 2
;initial        square  cube    apply-cube-changes      apply-square-changes
; 10            10(100) 10(100,1000) 1000(100)          100
;
;case 3
;initial        cube    apply-cube-changes  square  apply-square-changes
; 10            10(1000)    1000            1000(1000000)   1000000
;
;case 4
;initial        cube    square  apply-cube-changes  apply-square-changes  
; 10            10(1000) 10(1000,100) 1000(100)         100
;
;case 5
;initial        cube    square  apply-square-changes    apply-cube-changes
; 10            10(1000) 10(1000,100) 100(1000)         1000
;
;Take interleave case into consideration
;10000 and 100000 is also possible

(define x 10)
(define s (make-serializer))
(parallel-execute (s (lambda [] (set! x (* x x))))
                  (s (lambda [] (set! x (* x x x)))))

;Parallel execution cases
;
;atomic procedure square-and-apply, cube-and-apply
;
;Time elapse ----------------------------------------------------------------->
;case 0
;initial        square-and-apply    cube-and-apply
; 10            100                 1000000
;
;case 1
;initial        cube-and-apply      square-and-apply
; 10            1000                1000000
;
;Since cube and square is linearize add equal, it donen't output different
;result
