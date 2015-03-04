#lang racket

;Problem:
;Given an absolute path for a file (Unix-style), simplify it.
;
;For example,
;path = "/home/"            => "/home"
;path = "/a/./b/../../c/"   => "/c"
;path = "/a/./b//../../c//" => "/c"

(define slash #\/) (define single-slash-slice (list slash))
(define dot #\.) (define single-dot-slice (list dot))
(define equal-single-dot-slice? (curry equal? single-dot-slice))
(define double-dot-slice (list dot dot))
(define equal-double-dot-slice? (curry equal? double-dot-slice))

(define [split lst s]
  (let ([neq? (negate (curry eq? s))])
    (define [iter r]
      (if [null? r] '() (cons (takef r neq?) (iter (cdr (dropf r neq?))))))

    (iter lst)))

(define [my-simplify-path path]
  (let* ([p-chars (string->list path)]
         [sliced-p (split p-chars slash)]
         [w/o-empty (filter-not null? sliced-p)]
         [w/o-single-dot (filter-not equal-single-dot-slice? w/o-empty)])
    (define [proc-double-dots remaining accum]
      (define [continue]
        (let ([f-slice (car remaining)] [r-slices (cdr remaining)])
          (if [and [equal-double-dot-slice? f-slice] [not [null? accum]]]
            (proc-double-dots r-slices (drop-right accum 1))
            (proc-double-dots r-slices (append accum (list f-slice))))))

      (if [null? remaining] accum (continue)))

    (list->string
      (apply append (add-between (proc-double-dots w/o-single-dot '())
                                 single-slash-slice
                                 #:before-first (list single-slash-slice)
                                 #:splice? true)))))

(define test-path-a "/home/")
(define test-path-b "/a/./b/../../c/")
(define test-path-c "/a/./b//../../c//")

(my-simplify-path test-path-a)
(my-simplify-path test-path-b)
(my-simplify-path test-path-c)
