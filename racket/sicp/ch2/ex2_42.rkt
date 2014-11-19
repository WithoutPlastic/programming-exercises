#lang racket

(define [queens board-size]
  (define [queen-cols k]
    (if [= k 0]
      empty-board
      (filter
        (lambda (seq-of-positions)
          (safe? k seq-of-positions))
        (flatmap
          (lambda (rest-of-queens)
            (map
              (lambda (new-row)
                (adjoin-position new-row k rest-of-queens))
              (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(define empty-board '())

(define [adjoin-position new-row-pos row-index rest-of-queens]
  (append (list new-row-pos) rest-of-queens))

(define [safe? row-index seq-of-positions]
  (define [iter cur-col-index left-pos-seq]
    (cond ([null? left-pos-seq] #t)
          ([or
             (= cur-col-index (car left-pos-seq))
             (=
               (abs
                 (- cur-col-index (car left-pos-seq)))
               (abs
                 (- row-index (length left-pos-seq))))] #f)
          (else (iter cur-col-index (cdr left-pos-seq)))))
  (iter (car seq-of-positions) (cdr seq-of-positions)))
