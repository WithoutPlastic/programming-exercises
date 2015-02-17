#lang racket

;Problem:
;Given a collection of intervals, merge all overlapping intervals.
;
;For example,
;Given [1,3],[2,6],[8,10],[15,18],
;return [1,6],[8,10],[15,18].

(define [merge intervals]
  (let ([l-sorted (sort intervals (lambda [x y] [< (car x) (car y)]))])
    (define [iter remaining]
      (define [continue]
        (let* ([first-intvl (car remaining)]
               [fl (car first-intvl)] [fh (cdr first-intvl)]
               [second-intvl (cadr remaining)]
               [sl (car second-intvl)] [sh (cdr second-intvl)]
               [rest-intvls (cdr remaining)])
          (if [< fh sl]
            (cons first-intvl (iter rest-intvls))
            (iter (cons (cons (min fl sl) (max fh sh)) (cdr rest-intvls))))))

      (if [or [null? remaining] [null? (cdr remaining)]] remaining (continue)))

    (iter l-sorted)))

(define test-intervals (list '(1 . 3) '(2 . 6) '(8 . 10) '(15 . 18)))

(merge test-intervals)
