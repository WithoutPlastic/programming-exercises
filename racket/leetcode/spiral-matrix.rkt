#lang racket

;Problem:
;Given a matrix of m x n elements (m rows, n columns), return all elements of
;the matrix in spiral order.
;
;For example,
;Given the following matrix:
;
;[
;[ 1, 2, 3 ],
;[ 4, 5, 6 ],
;[ 7, 8, 9 ]
;]
;
;You should return [1,2,3,6,9,8,7,4,5].

(define [generate-spiral-idxs m n]
  (let* ([max-r-idx (sub1 m)] [max-c-idx (sub1 n)])
    (define [iter r]
      (let ([remaining-row (- m (* 2 r))]
            [remaining-col (- n (* 2 r))])
        (define [continue]
          (append (append (map (curry cons r) (range r (- n r)))
                          (map (curryr cons (- max-c-idx r))
                               (range (add1 r) (- m r)))
                          (map (curry cons (- max-r-idx r))
                               (range (sub1 (- max-c-idx r)) (sub1 r) -1))
                          (map (curryr cons r)
                               (range (sub1 (- max-r-idx r)) r -1)))
                  (iter (add1 r))))

        (cond ([and [< 1 remaining-row] [< 1 remaining-col]] (continue))
              ([= remaining-row 1] (map (curry cons r) (range r (- n r))))
              ([= remaining-col 1] (map (curryr cons r) (range r (- m r))))
              (else '()))))

    (iter 0)))
(provide generate-spiral-idxs)

(define [spiral-order matrix]
  (let* ([m (length matrix)] [n (length (car matrix))]
         [spiral-idxs (generate-spiral-idxs m n)])
    (map (lambda [p] (list-ref (list-ref matrix (car p)) (cdr p)))
         spiral-idxs)))

(define test-matrix-a (list '(1 2 3) '(4 5 6) '(7 8 9)))
(define test-matrix-b
  (list '(1 2 3 4 5 6) '(14 15 16 17 18 7) '(13 12 11 10 9 8)))
(define test-matrix-c
  (list '(1 2 3 4 5 6) '(16 17 18 19 20 7) '(15 24 23 22 21 21 8)
        '(14 13 12 11 10 9)))

;(spiral-order test-matrix-a)
;(spiral-order test-matrix-b)
;(spiral-order test-matrix-c)
