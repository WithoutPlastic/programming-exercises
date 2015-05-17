#lang racket

;Problem:
;Given n stick, each one with length ai. Please find out the possible stick combination which make
;triangle perimeter longest. Return zero if not found.
;
;For example:
;stick length: 2, 3, 4, 5, 10. Longest perimeter is 3 + 4 + 5 = 12.
;
;Restriction:
;3 <= n <= 100
;1 <= ai <= 1000000

(define [find-triangle-with-longest-perimeter length-list]
  (define not-found-value 0)

  (define [with-first-border first-length length-list]
    (if [< (length length-list) 2] not-found-value
      (let* ([second-length (car length-list)]
             [third-length (cadr length-list)]
             [remaining-length-list (cdr length-list)]
             [result (with-all-border first-length second-length third-length)])
        (if [not [= result not-found-value]] result
          (with-first-border second-length remaining-length-list)))))

  (define [with-all-border first-length second-length third-length]
    (if [<= (+ second-length third-length) first-length] not-found-value
      (+ first-length second-length third-length)))

  (let* ([descending-length-list (sort length-list >)]
         [longest-length (car descending-length-list)]
         [remaining-length-list (cdr descending-length-list)])
    (with-first-border longest-length remaining-length-list)))

(find-triangle-with-longest-perimeter (list 2 3 4 5 10))
(find-triangle-with-longest-perimeter (list 4 5 10 20))
