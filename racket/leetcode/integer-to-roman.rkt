#lang racket

;Problem
;Given an integer, convert it to a roman numeral.
;
;Input is guaranteed to be within the range from 1 to 3999.

(define [repeat-char char times]
  (if [< 0 times] (cons char (repeat-char char (sub1 times))) '()))

(define [integer->roman int]
  (define stages
    (list (list 1000 #\M #\? #\?)
          (list 100  #\C #\D #\M)
          (list 10   #\X #\L #\C)
          (list 1    #\I #\V #\X)))

  (define [expand remaining idx]
    (define [continue]
      (let* ([cur-stage (list-ref stages idx)]
             [cur-num (car cur-stage)]
             [cur-char (cadr cur-stage)]
             [lhf-char (caddr cur-stage)]
             [llv-char (cadddr cur-stage)])
        (define [proc]
          (let ([r (floor (/ remaining cur-num))])
            (cond ([= r 9] (list cur-char llv-char))
                  ([< 4 r] (cons lhf-char (repeat-char cur-char (- r 5))))
                  ([= r 4] (list cur-char lhf-char))
                  (else (repeat-char cur-char r)))))

        (append (proc)
                (expand (remainder remaining cur-num) (add1 idx)))))

    (if [< idx (length stages)] (continue) '()))

  (list->string (expand int 0)))

(integer->roman 0)
(integer->roman 1)
(integer->roman 2)
(integer->roman 3)
(integer->roman 4)
(integer->roman 5)
(integer->roman 6)
(integer->roman 7)
(integer->roman 8)
(integer->roman 9)
(integer->roman 10)
(integer->roman 15)
(integer->roman 23)
(integer->roman 39)
(integer->roman 49)
(integer->roman 50)
(integer->roman 83)
(integer->roman 90)
(integer->roman 100)
(integer->roman 500)
(integer->roman 550)
(integer->roman 707)
(integer->roman 890)
(integer->roman 1500)
(integer->roman 1800)
(integer->roman 900)
(integer->roman 3999)
