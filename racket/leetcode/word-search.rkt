#lang racket

;Problem:
;Given a 2D board and a word, find if the word exists in the grid.
;
;The word can be constructed from letters of sequentially adjacent cell, where
;"adjacent" cells are those horizontally or vertically neighboring. The same
;letter cell may not be used more than once.
;
;For example,
;Given board =
;
;[
;["ABCE"],
;["SFCS"],
;["ADEE"]
;]
;
;word = "ABCCED", -> returns true,
;word = "SEE", -> returns true,
;word = "ABCB", -> returns false.

(define [no-duplicates? lst] [= (length (remove-duplicates lst)) (length lst)])

(define [all-pairs-neighbor? ps]
  (define [continue]
    (let* ([f-pair (car ps)] [f-row (car f-pair)] [f-col (cdr f-pair)]
           [s-pair (cadr ps)] [r-pairs (cdr ps)]
           [f-neighbors
             (list (cons (add1 f-row) f-col) (cons (sub1 f-row) f-col)
                   (cons f-row (add1 f-col)) (cons f-row (sub1 f-col)))])
      [and [member s-pair f-neighbors] [all-pairs-neighbor? r-pairs]]))

  (if [or [null? ps] [null? (cdr ps)]] true (continue)))

(define [all-matrix-idx-pairs m n]
  (append-map (lambda [r] (map (lambda [c] (cons r c)) (range 0 n)))
              (range 0 m)))

(define [matrix-elt-eq? matrix p val]
  [eq? (list-ref (list-ref matrix (car p)) (cdr p)) val])

(define [exist? board word]
  (let* ([chars-board (map string->list board)] [w-chars (string->list word)]
         [m (length chars-board)] [n (length (car chars-board))]
         [all-idx-pairs (all-matrix-idx-pairs m n)]
         [bd-elt-eq? (curry matrix-elt-eq? chars-board)]
         [c-pos-lst (lambda [c] (filter (curryr bd-elt-eq? c) all-idx-pairs))]
         [w-char-positions (map c-pos-lst w-chars)])
    [not
      [null?
        (filter
          (lambda [x] [and [all-pairs-neighbor? x] [no-duplicates? x]])
          (foldl
            (lambda [ps accum]
              (append-map (lambda [o] (map (lambda [n] (cons n o)) ps)) accum))
            (list '())
            w-char-positions))]]))

(define test-broad (list "ABCE" "SFCS" "ADEE"))
(define word-a "ABCCED")
(define word-b "SEE")
(define word-c "ABCB")

(exist? test-broad word-a)
(exist? test-broad word-b)
(exist? test-broad word-c)
