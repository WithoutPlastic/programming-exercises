#lang racket

;Problem(POJ 3253):
;Farmer need to repair fence, need to cut long wood into N slice. The slice length is L1, L2, ...
;Ln. The un-cutted wood length is exactly equal to sliced length sum. When cutting wood, the
;overhead is the length of the wood.
;
;For example:
;Wood with length 21 to be cutted into slices with length 5, 8, 8.
;When first operation into slice length 13, 8, the overhead equal to 21.
;Then cut left longer one into 5, 8, the overhead equal to 13.
;Total overhead equal to 34.
;
;Please write program to calculate min overhead when cutting wood into required slice length.
;
;Restriction:
;1 <= N <= 20000
;0 <= Li <= 50000


(define [find-min-overhead len-lst]
  (if [<= [length len-lst] 1] 0
    (let iter ([overhead 0] [remaining-len-lst (sort len-lst <)])
      (if [null? (cdr remaining-len-lst)] overhead
        (let ([smallest-two-sum (+ (first remaining-len-lst) (second remaining-len-lst))])
          (iter (+ overhead smallest-two-sum)
                (sort (cons smallest-two-sum (cddr remaining-len-lst)) <)))))))


(find-min-overhead '(9 5 8))
