#lang racket

(define [count-pairs x]
  (if [not (pair? x)]
    0
    (+ (count-pairs (car x))
       (count-pairs (cdr x))
       1)))

(define [alt-count-pairs x]
  (define founded-pairs '())
  (define [iter pairs]
    (cond ([not (pair? pairs)] 0)
          ([memq pairs founded-pairs] 0)
          (else
            (begin
              (if [null? founded-pairs]
                (set! founded-pairs (list pairs))
                (set! founded-pairs (cons founded-pairs pairs)))
              (+ (iter (car pairs))
                 (iter (cdr pairs))
                 1)))))
  (iter x))

(define z3 '(a b c))

(define x '(a))
(define z4 (list x x))

(define y (cons x x))
(define z7 (cons y y))

;immutable data, not accept set-cdr!
;(define zi '(a b c d))
;(set-cdr! (cdddr zi) zi)

(count-pairs z3)
(count-pairs z4)
(count-pairs z7)

(alt-count-pairs z3)
(alt-count-pairs z4)
(alt-count-pairs z7)
