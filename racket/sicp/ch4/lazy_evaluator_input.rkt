(define [cons x y] (lambda [m] (m x y)))
(define [car z] (z (lambda [p q] p)))
(define [cdr z] (z (lambda [p q] q)))

(define [list-ref items n]
  (if [= n 0]
    (car items)
    (list-ref (cdr items) (- n 1))))

(define [add-lists list-a list-b]
  (cond ([null? list-a] list-b)
        ([null? list-b] list-a)
        (else (cons (+ (car list-a) (car list-b))
                    (add-lists (cdr list-a) (cdr list-b))))))

(define ones (cons 1 ones))
(define integers (cons 1 (add-lists ones integers)))

(list-ref integers 17)

(define [map proc items]
  (if [null? items]
    '()
    (cons (proc (car items))
          (map proc (cdr items)))))
(define [scale-list items factor] (map (lambda [x] (* x factor)) items))
(define [integral integrand initial-value dt]
  (define int
    (cons initial-value
          (add-lists (scale-list integrand dt)
                     int)))
  int)

(define [solve f y0 dt]
  (define y (integral dy y0 dt))
  (define dy (map f y))
  y)

(list-ref (solve (lambda [x] x) 1 0.001) 1000)

