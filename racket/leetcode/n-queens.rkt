#lang racket

;Problem:
;The n-queens puzzle is the problem of placing n queens on an n×n chessboard
;such that no two queens attack each other.
;
;
;
;Given an integer n, return all distinct solutions to the n-queens puzzle.
;
;Each solution contains a distinct board configuration of the n-queens'
;placement, where 'Q' and '.' both indicate a queen and an empty space
;respectively.
;
;For example,
;There exist two distinct solutions to the 4-queens puzzle:
;
;[
;[".Q..",  // Solution 1
; "...Q",
; "Q...",
; "..Q."],
;
;["..Q.",  // Solution 2
; "Q...",
; "...Q",
; ".Q.."]
;]

(define [generate-result q-positions n]
  (define [gen-row pos]
    (let ([col-idx (cdr pos)])
      (append (make-list col-idx #\.)
              (list #\Q)
              (make-list (sub1 (- n col-idx)) #\.))))

  (let ([pos-row-eq? (lambda [p r] [= (car p) r])])
    (define [iter row]
      (if [= row n]
        '()
        (append (map gen-row (filter (curryr pos-row-eq? row) q-positions))
                (iter (add1 row)))))

    (iter 0)))

(define [print-result result]
  (displayln "-- solution --")
  (map displayln result))

(define [all-possible-positions n]
  (append-map (lambda [x] (map (lambda [y] (cons x y))
                               (range 0 n)))
              (range 0 n)))

(define [available-positions ps all-possibles]
  (define [iter remaining selections]
    (define [continue]
      (let* ([first-pos (car remaining)] [p-row (car first-pos)]
             [p-col (cdr first-pos)] [rest-poss (cdr remaining)])
        (iter rest-poss
              (filter-not
                (lambda [x] [= (abs (- p-row (car x))) (abs (- p-col (cdr x)))])
                (filter-not
                  (lambda [x] [= (cdr x) p-col])
                  (filter-not
                    (lambda [x] [= (car x) p-row])
                    selections))))))

    (if [null? remaining] selections (continue)))

  (iter ps all-possibles))

(define [solve-n-queens n]
  (let ([all-positions (all-possible-positions n)])
    (define [iter results row]
      (if [< row n]
        (iter
          (append-map (lambda [s]
                        (map (curryr cons s)
                             (filter (lambda [x] [= (car x) row])
                                     (available-positions s all-positions))))
                      results)
          (add1 row))
        results))

    (iter (list '()) 0)))

(for-each (compose print-result (curryr generate-result 4)) (solve-n-queens 4))
(for-each (compose print-result (curryr generate-result 8)) (solve-n-queens 8))
