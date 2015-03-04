#lang racket

;Problem:
;Given a 2D binary matrix filled with 0's and 1's, find the largest rectangle
;containing all ones and return its area.

(define [maximal-rectangle matrix]
  (define [generate-all-idxs m n]
    (append-map (λ [r] (map (λ [c] (cons r c)) (range 0 n))) (range 0 m)))

  (define [matrix-v row col] (list-ref (list-ref matrix row) col))

  (define [result-max . results]
    (let* ([max-area (apply max (map cdr results))])
      (ormap (λ [r] [and [= (cdr r) max-area] r]) results)))

  (define [continue]
    (let* ([m (length matrix)] [n (length (car matrix))]
           [all-idxs (generate-all-idxs m n)]
           [f-0-idx (findf (λ [p] [= (matrix-v (car p) (cdr p)) 0]) all-idxs)])
      (define [split]
        (let ([zero-row (car f-0-idx)] [zero-col (cdr f-0-idx)])
          (result-max
            (maximal-rectangle (take matrix zero-row))
            (maximal-rectangle (drop matrix (add1 zero-row)))
            (maximal-rectangle (map (λ [line] (take line zero-col)) matrix))
            (maximal-rectangle (map (λ [line] (drop line (add1 zero-col)))
                                    matrix)))))

      (if f-0-idx (split) (cons matrix (* m n)))))

  (if [null? matrix] (cons matrix 0) (continue)))

(define [print-result result]
  (display "Area: ")
  (displayln (cdr result))
  (displayln "Matrix: ")
  (for-each displayln (car result)))

(define test-rectangle-a (make-list 6 '(0 0 0 0 0 0)))
(define test-rectangle-b
  (list '(0 1 1 1 1 1)
        '(1 1 1 1 1 1)
        '(1 1 1 1 1 1)
        '(1 1 1 1 1 1)
        '(1 1 1 1 1 1)
        '(1 1 1 1 1 1)
        '(1 1 1 1 1 0)))

(print-result (maximal-rectangle test-rectangle-a))
(newline)
(print-result (maximal-rectangle test-rectangle-b))
