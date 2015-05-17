#lang racket

;Problem:
;Given n stick, each one with length ai. Please find out the possible stick combination which make
;triangle perimeter longest.
;
;For example:
;stick length: 2, 3, 4, 5, 10. Longest perimeter is 3 + 4 + 5 = 12.
;
;Restriction:
;3 <= n <= 100
;1 <= ai <= 1000000

(define [find-triangle-with-longest-perimeter length-list]
  (define [with-first-border first-length length-list]
    (if [null? length-list] 0
      (let* ([second-length (car length-list)]
             [remaining-length-list (cdr length-list)]
             [result (with-second-border first-length second-length remaining-length-list)])
        (if [not [= result 0]] result
          (with-first-border second-length remaining-length-list)))))

  (define [with-second-border first-length second-length length-list]
    (if [null? length-list] 0
      (let* ([third-length (car length-list)]
             [remaining-length-list (cdr length-list)]
             [result (with-third-border first-length second-length third-length)])
        (if [not [= result 0]] result
          (with-second-border first-length third-length remaining-length-list)))))

  (define [with-third-border first-length second-length third-length]
    (if [<= (+ second-length third-length) first-length] 0
      (+ first-length second-length third-length)))

  (let* ([descending-length-list (sort length-list >)]
         [longest-length (car descending-length-list)]
         [remaining-length-list (cdr descending-length-list)])
    (with-first-border longest-length remaining-length-list)))

(find-triangle-with-longest-perimeter (list 2 3 4 5 10))
(find-triangle-with-longest-perimeter (list 4 5 10 20))
