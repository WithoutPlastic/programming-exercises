#lang racket

;Problem:
;Given a collection of intervals, merge all overlapping intervals.
;
;For example,
;Given [1,3],[2,6],[8,10],[15,18],
;return [1,6],[8,10],[15,18].

(require "insert-interval.rkt")
(define [insert-interval new-interval intervals]
  (insert intervals new-interval))

(define [merge intervals] (foldl insert-interval '() intervals))

(define test-intervals (list '(1 . 3) '(2 . 6) '(8 . 10) '(15 . 18)))

(merge test-intervals)
