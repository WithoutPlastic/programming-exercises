#lang racket

(define [make-point x y] (cons x y))
(define [point-x point] (car point))
(define [point-y point] (cdr point))
(define [inline-print-point point]
  (display "(")
  (display (point-x point))
  (display ",")
  (display (point-y point))
  (display ")"))
(define [print-point point]
  (newline)
  (inline-print-point point))

(define [make-segment point-a point-b] (cons point-a point-b))
(define [segment-start segment] (car segment))
(define [segment-end segment] (cdr segment))
(define [inline-print-segment segment]
  (inline-print-point (segment-start segment))
  (display " -> ")
  (inline-print-point (segment-end segment)))
(define [print-segment segment]
  (newline)
  (inline-print-segment segment))

(define [segment-mid-point segment]
  (let ([avg (lambda (x y) (/ (+ x y) 2))])
    (make-point
      (avg
        (point-x (segment-start segment))
        (point-x (segment-end segment)))
      (avg
        (point-y (segment-start segment))
        (point-y (segment-end segment))))))

(print-point (cons 1 2))
(print-segment (cons (cons 1 2) (cons 3 4)))
