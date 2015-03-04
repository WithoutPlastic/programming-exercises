#lang racket

;Problem:
;Given an unsorted array of integers, find the length of the longest
;consecutive elements sequence.
;
;For example,
;Given [100, 4, 200, 1, 3, 2],
;The longest consecutive elements sequence is [1, 2, 3, 4].
;
;Return its length: 4.
;
;Your algorithm should run in O(n) complexity.

(define [list-elt-idx pred? lst]
  (define [iter remaining ret]
    (cond ([null? remaining] false)
          ([pred? (car remaining)] ret)
          (else (iter (cdr remaining) (add1 ret)))))

  (iter lst 0))

(define [longest-consecutive ints]
  (let* ([len (length ints)]
         [one-based-ints (map (compose add1 (curryr - (apply min ints))) ints)]
         [slot-len (floor (/ (apply max one-based-ints) len))]
         [idx->slot-range?
           (λ [i] (λ [n] [and [<= (* i len) n] [< n (* (+ i 2) len)]]))]
         [get-slot-elts (λ [i] (filter (idx->slot-range? i) one-based-ints))]
         [indexing-counts (build-list slot-len get-slot-elts)]
         [candidates (argmax length indexing-counts)])
    (define [search candidates]
      (let ([to (apply max candidates)])
        (define [iter from cnt max-cnt]
          (cond ([< to from] (max cnt max-cnt))
                ([memq from candidates] (iter (add1 from) (add1 cnt) max-cnt))
                (else (iter (add1 from) 0 (max cnt max-cnt)))))

        (iter (apply min candidates) 0 0)))

    (search candidates)))

(longest-consecutive '(100 4 200 1 3 2))
(let ([test-ints (remove-duplicates (build-list 15 (thunk* (random 10))))])
  (displayln test-ints)
  (longest-consecutive test-ints))
