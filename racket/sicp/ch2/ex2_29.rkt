#lang racket

(define [make-mobile left right] (list left right))
(define [mobile-left m] (car m))
(define [mobile-right m] (cadr m))

(define [make-branch len structure] (list len structure))
(define [branch-length b] (car b))
(define [branch-structure b] (cadr b))

(define [branch? x] (number? (car x)))
(define [branch-with-weight? x]
  (and (branch? x) (number? (branch-structure x))))
(define [mobile? x] (not (branch? x)))

;Assuming mobile never following mobile
;Assuming branch never following branch
;Assuming mobile structure alway met first
(define [total-weight object]
  (cond ([null? object] 0)
        ([branch-with-weight? object] (branch-structure object))
        ([branch? object] (total-weight (branch-structure)))
        ([mobile? object]
         (+
           (total-weight (mobile-left object))
           (total-weight (mobile-right object))))))

(define [balanced? object]
  (define [inner-balanced? obj]
    (cond
      ([mobile? obj]
       (and
         (=
             (*
               (branch-length (mobile-left obj))
               (total-weight (mobile-left obj)))
             (*
               (branch-length (mobile-right))
               (total-weight (mobile-right))))
         (inner-balanced? (branch-structure (mobile-left obj)))
         (inner-balanced? (branch-structure (mobile-right obj)))))
      ([branch-with-weight?] #t)))
  (and
    [mobile? object]
    (inner-balanced? object)))
