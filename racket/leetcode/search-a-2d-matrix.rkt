#lang racket

;Problem:
;Write an efficient algorithm that searches for a value in an m x n matrix. This
;matrix has the following properties:
;
;Integers in each row are sorted from left to right.
;The first integer of each row is greater than the last integer of the previous
;row.
;
;For example, consider the following matrix:
;
;[
;[1,   3,  5,  7],
;[10, 11, 16, 20],
;[23, 30, 34, 50]
;]
;
;Given target = 3, return true.

(define [search-matrix matrix target]
  (let ([min-elt (caar matrix)] [max-elt (last (last matrix))])
    (define [iter-col row]
      (if [null? row] false [or [= target (car row)] (iter-col (cdr row))]))

    (define [iter-row matrix]
      (let ([first-row (car matrix)] [rest-rows (cdr matrix)])
        (define [continue]
          (let ([second-row-f-elt (caadr matrix)])
            (if [< target second-row-f-elt]
              (iter-col first-row)
              (iter-row rest-rows))))

        (if [null? rest-rows] (iter-col first-row) (continue))))

    (if [or [null? matrix] [< target min-elt] [< max-elt target]]
      false (iter-row matrix))))

(define test-matrix (list '(1 3 5 7) '(10 11 16 20) '(23 30 34 50)))

(search-matrix test-matrix 3)
(search-matrix test-matrix 21)
