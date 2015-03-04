#lang racket

;Problem:
;Determine if a Sudoku is valid, according to: Sudoku Puzzles - The Rules.
;
;The Sudoku board could be partially filled, where empty cells are filled with
;the character '.'.
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
;A partially filled sudoku which is valid.
;
;Note:
;A valid Sudoku board (partially filled) is not necessarily solvable. Only the
;filled cells need to be validated.)

(define blank #\.)

(define [sp-into-3 lst]
  (map (lambda [dp tk] (take (drop lst dp) tk))
       (range 0 7 3) (make-list 3 3)))

(define [valid-sudoku? sudoku]
  (let ([blank-or-1-9? (lambda [x] [or [eq? x blank] [and [< 0 x] [< x 10]]])]
        [blank? (lambda [x] [eq? x blank])]
        [no-dps? (lambda [l] [= (length l) (length (remove-duplicates l))])]
        [m-tp (lambda [m] (apply map list m))])
    [and [andmap (lambda [ln] [andmap blank-or-1-9? ln]) sudoku]
         [andmap (lambda [ln] [no-dps? (filter-not blank? ln)]) sudoku]
         [andmap (lambda [ln] [no-dps? (filter-not blank? ln)]) (m-tp sudoku)]
         [andmap (lambda [m] [no-dps? (filter-not blank? (flatten m))])
                 (append-map sp-into-3 (map m-tp (sp-into-3 sudoku)))]]))

(define test-sudoku
  (list (list 5 3 blank blank 7 blank blank blank blank)
        (list 6 blank blank 1 9 5 blank blank blank)
        (list blank 9 8 blank blank blank blank 6 blank)
        (list 8 blank blank blank 6 blank blank blank 3)
        (list 4 blank blank 8 blank 3 blank blank 1)
        (list 7 blank blank blank 2 blank blank blank 6)
        (list blank 6 blank blank blank blank 2 8 blank)
        (list blank blank blank 4 1 9 blank blank 5)
        (list blank blank blank blank 8 blank blank 7 9)))

(valid-sudoku? test-sudoku)
