#lang racket

(define [permute lst]
  (cond ([null? lst] '())
        ([= (length lst) 1] (list lst))
        (else (append-map (λ [x] (map (curry cons x) (permute (remove x lst))))
                          lst))))

(define [unique-permute lst]
  (cond ([null? lst] '())
        ([= (length lst) 1] (list lst))
        (else (append-map
                (λ [x] (map (curry cons x) (unique-permute (remove x lst))))
                (remove-duplicates lst)))))

(define [pick-n-elts lst n]
  (cond ([= n 0] (list '()))
        ([or [< n 0] [< (length lst) n]] '())
        (else (append (map (curry cons (car lst)) (pick-n-elts (cdr lst) (- n 1)))
                      (pick-n-elts (cdr lst) n)))))

(define [drop-n-elts lst n]
  (cond ([= n 0] (list lst))
        ([or [< n 0] [< (length lst) n]] '())
        (else (append (map (curry cons (car lst)) (drop-n-elts (cdr lst) n))
                      (drop-n-elts (cdr lst) (sub1 n))))))

(provide (all-defined-out))
