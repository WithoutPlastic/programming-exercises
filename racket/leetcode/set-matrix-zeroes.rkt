#lang racket

;Problem:
;Given a m x n matrix, if an element is 0, set its entire row and column to 0.
;Do it in place.

(define [generate-all-idx-pairs row col]
  (append-map (lambda [r] (map (lambda [c] (cons r c)) (range 0 col)))
              (range 0 row)))

(define [set-zeroes! mat]
  (let* ([m (vector-length mat)]
         [n (vector-length (vector-ref mat 0))]
         [all-idx-pairs (generate-all-idx-pairs m n)])
    (for-each
      (lambda [p]
        (vector-fill! (vector-ref mat (car p)) 0)
        (vector-map! (lambda [v] (vector-set! v (cdr p) 0) v) mat))
      (filter (lambda [p] [= (vector-ref (vector-ref mat (car p)) (cdr p)) 0])
              all-idx-pairs))))

(define test-matrix (vector (vector 1 1 1 1 1) (vector 1 1 1 0 1)
                            (vector 1 1 0 1 1) (vector 1 1 1 1 1)))

(set-zeroes! test-matrix)
(for-each (lambda [r] (displayln (vector-ref test-matrix r)))
          (range 0 (vector-length test-matrix)))
