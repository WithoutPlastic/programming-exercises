#lang racket

(define [accumulate op initial sequence]
  (if [null? sequence]
    initial
    (op
      (car sequence)
      (accumulate op initial (cdr sequence)))))

(define [accumulate-n op init seqs]
  (if [null? (car seqs)]
    '()
    (cons (accumulate op init (map car seqs))
          (accumulate-n op init (map cdr seqs)))))

(accumulate
  append
  '()
  (map
    (lambda (i)
      (map
        (lambda (j) (list i j))
        enumerate-interval 1 (- i 1)))
    (enumerate-interval 1 n)))

(define [flatmap proc seq]
  (accumulate append '() (map proc seq)))

(define [prime-sum? pair]
  (prime? (+ (car pair) (cadr pair))))

(define [make-pair-sum pair]
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define [prime-sum-pairs n]
  (map
    make-pair-sum
    (filter
      prime-sum?
      (unique-pairs n))))
;      (flatmap
;        (lambda (i)
;          (map
;            (lambda (j) (list i j))
;            (enumerate-interval 1 (- i 1))))
;        (enumerate-interval 1 n)))))

(define [unique-pairs n]
  (flatmap
    (lambda (i)
      (map
        (lambda (j) (list i j))
        (enumerate-interval 1 (- i 1)))
      (enumerate-interval 1 n))))

(define [permutations s]
  (if [null? s]
    '()
    (flatmap
      (lambda (x)
        (map
          (lambda (p)
            (cons x p))
          (permutations (remove x s))))
      s)))

(define [remove item sequence]
  (filter
    (lambda (x)
      (not (= x item)))
    sequence))

(define [triple-sum-equal-filter n s]
  (filter (lambda (x) (= n (+ car x) (cadr x) (caddr x)))
          (unique-triples n)))

(define [unique-triples n]
  (accumulate
    append
    '()
    (flatmap
      (lambda (i)
        (map
          (lambda (j)
            (map
              (lambda (k) (list i j k)
              (enumerate-interval 1 (- j 1)))))
          (enumerate-interval 1 (- i 1))))
      (enumerate-interval 1 n))))
