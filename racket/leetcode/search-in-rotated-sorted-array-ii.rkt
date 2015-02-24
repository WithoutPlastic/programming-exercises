#lang racket

;Problem:
;Follow up for "Search in Rotated Sorted Array":
;What if duplicates are allowed?
;
;Would this affect the run-time complexity? How and why?
;
;Write a function to determine if a given target is in the array.

(define [search? rs-nums target]
  (let ([unique-nums (remove-duplicates rs-nums)])
    (define [iter remaining]
      (let ([len (length remaining)])
        (define [continue]
          (let* ([ln (first remaining)] [rn (last remaining)]
                 [mid-idx (floor (/ len 2))]
                 [mn (list-ref remaining mid-idx)]
                 [lp (take (drop remaining 1) (sub1 mid-idx))]
                 [rp (drop-right (drop remaining (add1 mid-idx)) 1)])
            (cond ([or [= target ln] [= target rn] [= target mn]] true)
                  ([< ln mn]
                   (if [and [< target mn] [< ln target]] (iter lp) (iter rp)))
                  ([and [< mn target] [< target rn]] (iter rp))
                  (else (iter lp)))))

        (cond ([= len 0] false)
              ([= len 1] [= (car remaining) target])
              ([= len 2] [or [= (car remaining) target] [= (cadr remaining) target]])
              (else (continue)))))

    (iter unique-nums)))

(define test-nums-a '(1 2 3 4 5 6 7 -4 -3 -2 -1 0))
(search? test-nums-a 8)
