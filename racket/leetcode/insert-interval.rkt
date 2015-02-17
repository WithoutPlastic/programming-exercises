#lang racket

;Problem:
;Given a set of non-overlapping intervals, insert a new interval into the
;intervals (merge if necessary).
;
;You may assume that the intervals were initially sorted according to their
;start times.
;
;Example 1:
;Given intervals [1,3],[6,9], insert and merge [2,5] in as [1,5],[6,9].
;
;Example 2:
;Given [1,2],[3,5],[6,7],[8,10],[12,16], insert and merge [4,9] in as
;[1,2],[3,10],[12,16].
;
;This is because the new interval [4,9] overlaps with [3,5],[6,7],[8,10].

(define [between? interval target]
  [and [<= (car interval) target] [<= target (cdr interval)]])

(define [insert intervals new-interval]
  (let ([nl (car new-interval)] [nh (cdr new-interval)])
    (define [iter remaining accum]
      (define [continue]
        (let* ([f-interval (car remaining)] [fl (car f-interval)]
               [fh (cdr f-interval)] [r-intervals (cdr remaining)])
          (cond ([and [between? f-interval nl] [between? f-interval nh]]
                 (append accum remaining))
                ([and [< nl fl] [< nh fl]]
                 (append accum (list new-interval) remaining))
                ([and [< fh nl] [< fh nh]]
                 (iter r-intervals (append accum (list f-interval))))
                ([and [< nl fl] [between? f-interval nh]]
                 (append accum (cons (cons nl fh) r-intervals)))
                (else
                  (append accum (insert r-intervals (cons (min nl fl) nh)))))))

      (if [null? remaining]
        (append accum (list new-interval))
        (continue)))

    (iter intervals '())))

(define test-intervals-a (list '(1 . 3) '(6 . 9)))
(define test-new-interval-a '(2 . 5))

(define test-intervals-b (list '(1 . 2) '(3 . 5) '(6 . 7) '(8 . 10) '(12 . 16)))
(define test-new-interval-b '(4 . 9))

(define test-intervals-c (list '(1 . 2) '(3 . 5) '(6 . 7) '(8 . 10) '(12 . 16)))
(define test-new-interval-c '(0 . 20))

(define test-intervals-d (list '(1 . 2) '(3 . 5) '(6 . 7) '(8 . 10) '(14 . 16)))
(define test-new-interval-d '(11 . 13))

(insert test-intervals-a test-new-interval-a)
(insert test-intervals-b test-new-interval-b)
(insert test-intervals-c test-new-interval-c)
(insert test-intervals-d test-new-interval-d)
