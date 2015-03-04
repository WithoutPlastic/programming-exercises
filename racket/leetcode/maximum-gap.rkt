#lang racket

;Problem:
;Given an unsorted array, find the maximum difference between the successive
;elements in its sorted form.
;
;Try to solve it in linear time/space.
;
;Return 0 if the array contains less than 2 elements.
;
;You may assume all elements in the array are non-negative integers and fit in
;the 32-bit signed integer range.
;
;Credits:
;Special thanks to @porker2008 for adding this problem and creating all test
;cases.

(define [list-elt-idx pred? lst]
  (define [iter remaining ret]
    (cond ([null? remaining] false)
          ([pred? (car remaining)] ret)
          (else (iter (cdr remaining) (add1 ret)))))

  (iter lst 0))

(define [longest-pred-idxs pred? lst]
  (define [head-match from result]
    (define [tail-match to result]
      (cond ([<= (length lst) to] (cons (cons from (sub1 to)) result))
            ([pred? (list-ref lst to)] (tail-match (add1 to) result))
            (else (head-match (add1 to) (cons (cons from (sub1 to)) result)))))

    (cond ([<= (length lst) from] result)
          ([pred? (list-ref lst from)] (tail-match (add1 from) result))
          (else (head-match (add1 from) result))))

  (let* ([idx-pairs (head-match 0 '())]
         [max-pair (argmax (λ [p] (- (cdr p) (car p))) idx-pairs)]
         [max-len (- (cdr max-pair) (car max-pair))])
    (filter (λ [p] [= (- (cdr p) (car p)) max-len]) idx-pairs)))

(define [maximum-gap ints]
  (let* ([len (length ints)]
         [zero-based-ints (map (curryr - (apply min ints)) ints)]
         [step (add1 (floor (/ (apply max zero-based-ints) len)))]
         [idx->in-slot?
           (λ [i] (λ [n] [and [<= (* i step) n] [< n (* (add1 i) step)]]))]
         [get-slot-elts (λ [i] (filter (idx->in-slot? i) zero-based-ints))]
         [slots (map get-slot-elts (range 0 len))]
         [empty-idx-pairs (longest-pred-idxs null? slots)])
    (define [search idx-pair]
      (define [down low from-idx]
        (define [iter cnt]
          (if [memq (- low cnt) (list-ref slots (sub1 from-idx))]
            cnt (iter (add1 cnt))))

        (iter 0))
      (define [up high to-idx]
        (define [iter cnt]
          (if [memq (+ high cnt) (list-ref slots (add1 to-idx))]
            cnt (iter (add1 cnt))))

        (iter 0))

      (let* ([from-idx (car idx-pair)] [to-idx (cdr idx-pair)]
             [up-start (* step (add1 to-idx))]
             [down-start (sub1 (* step from-idx))]
             [base-gap (* step (add1 (- to-idx from-idx)))])
        (cond ([= from-idx 0] (+ base-gap (up up-start from-idx)))
              ([= to-idx (sub1 len)] (+ base-gap (down down-start to-idx)))
              (else (+ (add1 base-gap)
                       (up up-start from-idx)
                       (down down-start to-idx))))))

    (apply max (map search empty-idx-pairs))))

(define test-ints (build-list 10 (thunk* (random 50))))

(displayln test-ints)
(maximum-gap test-ints)
