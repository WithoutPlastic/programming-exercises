#lang racket

;Problem:
;Given numRows, generate the first numRows of Pascal's triangle.
;
;For example, given numRows = 5,
;Return
;
;[
;     [1],
;    [1,1],
;   [1,2,1],
;  [1,3,3,1],
; [1,4,6,4,1]
;]

(define [generate-pascal-triangle n]
  (define [iter prow cnt]
    (if [= 0 cnt] '()
      (let ([exted-row (map + (cons 0 prow) (reverse (cons 0 prow)))])
        (cons exted-row (iter exted-row (sub1 cnt))))))

  (when [< 0 n]
    (if [= n 1] (list '(1))
      (append (list '(1)) (iter '(1) (- n 1))))))

(generate-pascal-triangle 5)
