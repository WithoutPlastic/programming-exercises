#lang racket

;Problem:
;Given a sorted integer array where the range of elements are [0, 99]
;inclusive, return its missing ranges.
;
;For example,
;
;given [0, 1, 3, 50, 75]
;return [“2”, “4->49”, “51->74”, “76->99”]

(define [find-missing-range ints start end]
  (define [iter remaining]
    (if [null? (cdr remaining)] '()
      (let ([first-num (car remaining)] [second-num (cadr remaining)])
        (if [or [= first-num second-num] [= (add1 first-num) second-num]]
          (iter (cdr remaining))
          (cons (cons (add1 first-num) (sub1 second-num))
                (iter (cdr remaining)))))))

  (if [null? ints] (list (cons start (sub1 end)))
    (iter (append (cons (sub1 start) ints) (list end)))))

(define test-ints '(0 1 3 50 75))

(find-missing-range test-ints 0 100)
