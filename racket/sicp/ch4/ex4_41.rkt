#lang racket

(define [distinct? items]
  (cond ([null? items] true)
        ([null? (cdr items)] true)
        ([member (car items) (cdr items)] false)
        (else (distinct? (cdr items)))))

(define [flatmap proc items]
  (if [null? items]
    '()
    (let ([mapped (proc (car items))]
          [remaining (flatmap proc (cdr items))])
      (if [pair? mapped]
        (append mapped remaining)
        (cons mapped remaining)))))

(define [permutations lists]
  (if [null? lists]
    (list '())
    (flatmap (lambda [x] (map (lambda [y] (cons x y)) (permutations (cdr lists))))
             (car lists))))

(define [met-requirements? items]
  (let ([b (first items)]
        [c (second items)]
        [f (third items)]
        [m (fourth items)]
        [s (fifth items)])
    [and [not [= b 5]] [not [= c 1]] [not [= f 1]] [not [= f 5]] [< c m]
         [not [= (abs (- s f)) 1]]
         [not [= (abs (- f c)) 1]]
         [distinct? items]]))

(define [multi-dwelling]
  (let* ([baker '(1 2 3 4 5)]
         [cooper '(1 2 3 4 5)]
         [fletcher '(1 2 3 4 5)]
         [miller '(1 2 3 4 5)]
         [smith '(1 2 3 4 5)])
    (filter met-requirements?
            (permutations (list baker cooper fletcher miller smith)))))
(multi-dwelling)
