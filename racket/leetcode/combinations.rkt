#lang racket

;Problem:
;Given two integers n and k, return all possible combinations of k numbers out
;of 1 ... n.
;
;For example,
;If n = 4 and k = 2, a solution is:
;
;[
;[2,4],
;[3,4],
;[2,3],
;[1,2],
;[1,3],
;[1,4],
;]

(define [combine n k]
  (define [iter remaining r]
    (if [< 1 r]
      (append-map (lambda [x] (map (curry cons x)
                                   (iter (filter (curry < x) remaining)
                                         (sub1 r))))
                  remaining)
      (map list remaining)))

  (iter (range 1 (add1 n)) k))

(combine 4 2)
