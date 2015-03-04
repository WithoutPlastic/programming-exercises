#lang racket

;Problem:
;Merge k sorted linked lists and return it as one sorted list. Analyze and
;describe its complexity.

(define [merge-sorted-lists sorted-lists]
  (define [iter remainings]
    (let ([valids (filter-not null? remainings)])
      (define [continue]
        (let* ([first-elts (map car valids)]
               [min-num (apply min first-elts)]
               [eq-min? (lambda [x] [= x min-num])]
               [min-nums (filter eq-min? first-elts)]
               [next (map (lambda [x] (if [eq-min? (car x)] (cdr x) x))
                          valids)])
          (append min-nums (iter next))))

      (cond ([null? valids] '())
            ([= (length valids) 1] (car valids))
            (else (continue)))))
 
  (iter sorted-lists))

(define sorted-a '(-20 -14 -1 4 8 100))
(define sorted-b '(-24 -4 -1 1 23 89))
(define sorted-c '(-8 -2 -1 0 0 1 7 17 25 32 45 64 72 81 99))
(define sorteds (list sorted-a sorted-b sorted-c))

(merge-sorted-lists sorteds)
