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
      (let ([len (length row)])
        (define [binary-split]
          (let* ([mid-idx (floor (/ len 2))]
                 [mid-elt (list-ref row mid-idx)])
            (cond ([= mid-elt target] true)
                  ([< target mid-elt] (iter-col (take row mid-idx)))
                  (else (iter-col (drop row (add1 mid-idx)))))))

        (cond ([= len 1] [= (car row) target])
              ([= len 2] [or [= (car row) target] [= (cadr row) target]])
              (else (binary-split)))))

    (define [iter-row matrix]
      (let ([len (length matrix)])
        (define [binary-split]
          (let* ([mid-idx (floor (/ len 2))]
                 [mid-row (list-ref matrix mid-idx)]
                 [mid-row-f-elt (car mid-row)]
                 [mid-right-idx (add1 mid-idx)]
                 [mid-right-row (list-ref matrix mid-right-idx)]
                 [mid-right-f-elt (car mid-right-row)])
            (cond ([< target mid-row-f-elt] (iter-row (take matrix mid-idx)))
                  ([< target mid-right-f-elt] (iter-col mid-row))
                  (else (iter-row (drop matrix mid-right-idx))))))

        (cond ([= len 1] (iter-col (car matrix)))
              ([= len 2]
               (if [< target (caadr matrix)]
                 (iter-col (car matrix)) (iter-col (cadr matrix))))
              (else (binary-split)))))

    (if [or [null? matrix] [< target min-elt] [< max-elt target]]
      false (iter-row matrix))))

(define test-matrix
  (list '(1 2 3 4 5 7 8 9)
        '(10 11 14 15 16 17 19 20)
        '(23 30 34 41 48 50 55 60)
        '(80 90 111 120 180 200 212 213)))

(search-matrix test-matrix 55)
(search-matrix test-matrix 210)
