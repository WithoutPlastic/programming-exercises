#lang racket

;Problem:
;ven a 2D board containing 'X' and 'O', capture all regions surrounded by 'X'.
;
;A region is captured by flipping all 'O's into 'X's in that surrounded region.
;
;For example,
;
;X X X X
;X O O X
;X X O X
;X O X X
;
;After running your function, the board should be:
;
;X X X X
;X X X X
;X X X X
;X O X X

(define [gen-all-idx-pairs row col]
  (append-map (λ [r] (map (λ [c] (cons r c)) (range 0 col))) (range 0 row)))

(define [gen-outter-idx-pairs row col]
  (remove-duplicates (append (map (curry cons 0) (range 0 col))
                             (map (curryr cons 0) (range 0 row))
                             (map (curry cons (sub1 row)) (range 0 col))
                             (map (curryr cons (sub1 col)) (range 0 row)))))

(define [connecting-pairs p]
  (let ([r (car p)] [c (cdr p)])
    (list (cons (add1 r) c) (cons (sub1 r) c)
          (cons r (add1 c)) (cons r (sub1 c)))))

(define [generate row col zero-ps]
  (map (λ [r] (map (λ [c] (if [member (cons r c) zero-ps] 0 1))
                   (range 0 col)))
       (range 0 row)))

(define [solve board]
  (define [elt p] (list-ref (list-ref board (car p)) (cdr p)))

  (define [iter passes pendings]
    (if [null? pendings] board
      (let* ([pass-connects (append-map connecting-pairs passes)]
             [connect-to-passes? (curryr member pass-connects)]
             [intersects (filter connect-to-passes? pendings)]
             [lefts (filter-not connect-to-passes? pendings)])
        (if [null? intersects] passes
          (append passes (iter intersects lefts))))))

  (let* ([row (length board)] [col (length (car board))]
         [elt-zero-eq? (compose (curry eq? 0) elt)]
         [zero-ps (filter elt-zero-eq? (gen-all-idx-pairs row col))]
         [outter-zero-ps (filter elt-zero-eq? (gen-outter-idx-pairs row col))]
         [inner-zero-ps (filter-not (curryr member outter-zero-ps) zero-ps)])
    (if [null? outter-zero-ps] board
      (generate row col (iter outter-zero-ps inner-zero-ps)))))

(define test-board-a (list '(1 1 1 1) '(1 0 0 1) '(1 1 0 1) '(1 0 1 1)))
(define test-board-b (list '(1 1 1 1) '(0 0 1 1) '(0 1 0 1) '(0 1 1 1)))

(solve test-board-a)
(solve test-board-b)
