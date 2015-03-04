#lang racket

;Problem:
;Write a program to solve a Sudoku puzzle by filling the empty cells.
;
;Empty cells are indicated by the character '.'.
;
;You may assume that there will be only one unique solution.
;
;+---+---+---+---+---+---+---+---+---+
;| 5 | 3 |   |   | 7 |   |   |   |   |
;+---+---+---+---+---+---+---+---+---+
;| 6 |   |   | 1 | 9 | 5 |   |   |   |
;+---+---+---+---+---+---+---+---+---+
;|   | 9 | 8 |   |   |   |   | 6 |   |
;+---+---+---+---+---+---+---+---+---+
;| 8 |   |   |   | 6 |   |   |   | 3 |
;+---+---+---+---+---+---+---+---+---+
;| 4 |   |   | 8 |   | 3 |   |   | 1 |
;+---+---+---+---+---+---+---+---+---+
;| 7 |   |   |   | 2 |   |   |   | 6 |
;+---+---+---+---+---+---+---+---+---+
;|   | 6 |   |   |   |   | 2 | 8 |   |
;+---+---+---+---+---+---+---+---+---+
;|   |   |   | 4 | 1 | 9 |   |   | 5 |
;+---+---+---+---+---+---+---+---+---+
;|   |   |   |   | 8 |   |   | 7 | 9 |
;+---+---+---+---+---+---+---+---+---+
;
;A sudoku puzzle...
;
;+---+---+---+---+---+---+---+---+---+
;| 5 | 3 | 4 | 6 | 7 | 8 | 9 | 1 | 2 |
;+---+---+---+---+---+---+---+---+---+
;| 6 | 7 | 2 | 1 | 9 | 5 | 3 | 4 | 8 |
;+---+---+---+---+---+---+---+---+---+
;| 1 | 9 | 8 | 3 | 4 | 2 | 5 | 6 | 7 |
;+---+---+---+---+---+---+---+---+---+
;| 8 | 5 | 9 | 7 | 6 | 1 | 4 | 2 | 3 |
;+---+---+---+---+---+---+---+---+---+
;| 4 | 2 | 6 | 8 | 5 | 3 | 7 | 9 | 1 |
;+---+---+---+---+---+---+---+---+---+
;| 7 | 1 | 3 | 9 | 2 | 4 | 8 | 5 | 6 |
;+---+---+---+---+---+---+---+---+---+
;| 9 | 6 | 1 | 5 | 3 | 7 | 2 | 8 | 4 |
;+---+---+---+---+---+---+---+---+---+
;| 2 | 8 | 7 | 4 | 1 | 9 | 6 | 3 | 5 |
;+---+---+---+---+---+---+---+---+---+
;| 3 | 4 | 5 | 2 | 8 | 6 | 1 | 7 | 9 |
;+---+---+---+---+---+---+---+---+---+
;
;...and its solution numbers marked in red.

(define blank #\.)
(define [blank? x] [eq? x blank])

(define [sp-into-n lst n]
  (let* ([len (length lst)] [step (/ len n)])
    (map (lambda [dp tk] (take (drop lst dp) tk))
         (range 0 (add1 (* (sub1 n) step)) step)
         (make-list n step))))

(define [valid-sudoku? sudoku]
  (let* ([len (length sudoku)]
         [step (sqrt len)]
         [split (curryr sp-into-n step)]
         [blank-or-1-9?
           (lambda [x] [or [eq? x blank] [and [< 0 x] [< x (add1 len)]]])]
         [no-dps? (lambda [l] [= (length l) (length (remove-duplicates l))])]
         [m-tp (lambda [m] (apply map list m))])
    [and [andmap (lambda [ln] [andmap blank-or-1-9? ln]) sudoku]
         [andmap (lambda [ln] [no-dps? (filter-not blank? ln)]) sudoku]
         [andmap (lambda [ln] [no-dps? (filter-not blank? ln)]) (m-tp sudoku)]
         [andmap (lambda [m] [no-dps? (filter-not blank? (flatten m))])
                 (append-map split (map m-tp (split sudoku)))]]))

(define [gen-row-col-pairs len]
  (let ([rows (range 0 len)] [cols (range 0 len)])
    (append-map (lambda [r] (map (lambda [c] (cons r c)) cols)) rows)))

(define [sudoku-v sudoku row col] (list-ref (list-ref sudoku row) col))

(define [replace-elt lst idx v]
  (append (take lst idx) (list v) (drop lst (add1 idx))))
(define [extend-sudoku sudoku row col n]
  (replace-elt sudoku row (replace-elt (list-ref sudoku row) col n)))

(define [solve-sudoku sudoku]
  (let* ([len (length sudoku)]
         [row-col-pairs (gen-row-col-pairs len)]
         [bypass-if-ref-blank
           (lambda [p] [and [blank? (sudoku-v sudoku (car p) (cdr p))] p])]
         [blank-rc-pairs (filter bypass-if-ref-blank row-col-pairs)])
    (foldl
      (lambda [p sudokus]
        (displayln "Progressing")
        (filter
          valid-sudoku?
          (append-map
            (lambda [x]
              (map (lambda [sudoku] (extend-sudoku sudoku (car p) (cdr p) x))
                   sudokus))
            (range 1 (add1 len)))))
      (list sudoku)
      blank-rc-pairs)))

(define [print-sudoku sudoku] (newline) (map displayln sudoku))

(define test-sudoku-3
  (list (list 5 3 blank blank 7 blank blank blank blank)
        (list 6 blank blank 1 9 5 blank blank blank)
        (list blank 9 8 blank blank blank blank 6 blank)
        (list 8 blank blank blank 6 blank blank blank 3)
        (list 4 blank blank 8 blank 3 blank blank 1)
        (list 7 blank blank blank 2 blank blank blank 6)
        (list blank 6 blank blank blank blank 2 8 blank)
        (list blank blank blank 4 1 9 blank blank 5)
        (list blank blank blank blank 8 blank blank 7 9)))

(for-each print-sudoku (solve-sudoku test-sudoku-3))

(define b blank)
(define test-sudoku-4
  (list (list 6 b b b b 12 b 15 14 b 3 b b b b 5)
        (list b b b 11 1 7 6 9 4 12 8 5 3 b b b)
        (list b 1 b 7 b 5 b b b b 10 b 16 b 14 b)
        (list 10 b b 13 16 2 b b b b 11 6 15 b b 12)
        (list 13 b b 4 3 16 b b b b 1 8 9 b b 10)
        (list b 16 b 2 b 4 b b b b 14 b 12 b 1 b)
        (list b b b 1 15 9 5 7 2 13 12 16 6 b b b)
        (list 11 b b b b 8 b 12 7 b 15 b b b b 16)
        (list b b b 5 b 11 2 4 13 10 6 b 8 b b b)
        (list b 11 b b b b b b b b b b b b 16 b)
        (list 9 b 3 b b b 8 b b 15 b b b 7 b 2)
        (list b 2 b b 10 b b 1 5 b b 9 b b 12 b)
        (list b 8 b b 9 b b 3 10 b b 14 b b 6 b)
        (list 5 b 15 b b b 14 b b 1 b b b 12 b 4)
        (list b 12 b b b b b b b b b b b b 5 b)
        (list b b b 6 b 13 15 5 16 2 4 b 7 b b b)))

(for-each print-sudoku (solve-sudoku test-sudoku-4))
