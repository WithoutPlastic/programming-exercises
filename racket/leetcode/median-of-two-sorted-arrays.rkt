#lang racket

;Problem:
;There are two sorted arrays A and B of size m and n respectively. Find the
;median of the two sorted arrays. The overall run time complexity should be
;O(log (m+n)).

(define [min2avg nums-a nums-b]
  (define [smallest-two-avg smaller-nums nums]
    (/ (+ (car smaller-nums)
          (min (cadr smaller-nums) (car nums)))
       2))
  (if [<= (car nums-a) (car nums-b)]
    (smallest-two-avg nums-a nums-b)
    (smallest-two-avg nums-b nums-a)))

(define [normal-find-median-sorted-arrays numbers-m len-m numbers-n len-n]
  (let* ([len-sum (+ len-m len-n)]
         [parity (if [odd? len-sum] 'odd 'even)]
         [drop-cnt (if [odd? len-sum]
                     (floor (/ len-sum 2))
                     (sub1 (floor (/ len-sum 2))))])
    (define [iter remaining-m remaining-n remaining-cnt]
      (cond ([and [= remaining-cnt 0] [eq? parity 'odd]]
             (min (car remaining-m) (car remaining-n)))
            ([and [= remaining-cnt 0] [eq? parity 'even]]
             (min2avg remaining-m remaining-n))
            ([< (car remaining-m) (car remaining-n)]
             (iter (cdr remaining-m) remaining-n (sub1 remaining-cnt)))
            (else (iter remaining-m (cdr remaining-n) (sub1 remaining-cnt)))))
    (iter numbers-m numbers-n drop-cnt)))

(define [fast-find-median-sorted-arrays numbers-m len-m numbers-n len-n]
  (define [find-x-th-smallest remaining-m len-m remaining-n len-n x-th]
    (define [continue]
      (let* ([n-part-idx (min (floor (/ x-th 2)) len-n)]
             [m-part-idx (- x-th n-part-idx)]
             [cmp-n-num (list-ref remaining-n (sub1 n-part-idx))]
             [cmp-m-num (list-ref remaining-m (sub1 m-part-idx))])
        (cond ([< cmp-m-num cmp-n-num]
               (find-x-th-smallest (drop remaining-m m-part-idx)
                                   (- len-m m-part-idx)
                                   remaining-n
                                   len-n
                                   (- x-th m-part-idx)))
              ([< cmp-n-num cmp-m-num]
               (find-x-th-smallest (drop remaining-n n-part-idx)
                                   (- len-n n-part-idx)
                                   remaining-m
                                   len-m
                                   (- x-th n-part-idx)))
              (else cmp-m-num))))

    (cond ([< len-m len-n] (find-x-th-smallest remaining-n len-n
                                               remaining-m len-m
                                               x-th))
          ([= len-n 0] (list-ref remaining-m (sub1 x-th)))
          ([= x-th 1] (min (car remaining-m) (car remaining-n)))
          (else (continue))))

  (let ([len-sum (+ len-m len-n)])
    (if [odd? len-sum]
      (find-x-th-smallest numbers-m len-m
                          numbers-n len-n
                          (add1 (floor (/ len-sum 2))))
      (/ (+ (find-x-th-smallest numbers-m len-m
                                numbers-n len-n
                                (floor (/ len-sum 2)))
            (find-x-th-smallest numbers-m len-m
                                numbers-n len-n
                                (add1 (floor (/ len-sum 2)))))
         2))))

(define find-median-sorted-arrays fast-find-median-sorted-arrays)

(define numbers-m '(1 3 4 5 7 8 10 30))
(define len-m (length numbers-m))
(define numbers-n '(3 6 8 12 30 50))
(define len-n (length numbers-n))

(displayln (find-median-sorted-arrays numbers-m len-m numbers-n len-n))
