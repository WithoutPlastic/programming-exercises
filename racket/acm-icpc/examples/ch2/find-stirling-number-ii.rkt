#lang racket

;Problem:
;There is n kind of element, and i-th kind with count of ai. No difference between same kind
;element. Calculate remainder of M of the solution of picking m element from them.
;
;Restriction:
;1 <= n <= 1000
;1 <= m <= 1000
;1 <= ai <= 1000
;2 <= M <= 10000


(require "../../lib/memorize-function.rkt")


(define [find-stirling-number elt-lst m M]
  (define [iter elt-lst m]
    (cond ([or [< m 0] [null? elt-lst] [< (apply + elt-lst) m]] 0)
          ([or [= m 0] [= (apply + elt-lst) m]] 1)
          (else (+ (iter (cdr elt-lst) m)
                   (- (iter elt-lst (sub1 m))
                      (iter (cdr elt-lst) (- (sub1 m) (car elt-lst))))))))

  (unless [< (apply + elt-lst) 3]
    (remainder ((memorize-func iter) elt-lst m) M)))


(find-stirling-number (list 1 2 3) 3 10000)
