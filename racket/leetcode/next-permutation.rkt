#lang racket

;Problem:
;Implement next permutation, which rearranges numbers into the lexicographically
;next greater permutation of numbers.
;
;If such arrangement is not possible, it must rearrange it as the lowest
;possible order (ie, sorted in ascending order).
;
;The replacement must be in-place, do not allocate extra memory.
;
;Here are some examples. Inputs are in the left-hand column and its
;corresponding outputs are in the right-hand column.
;
;1,2,3 → 1,3,2
;3,2,1 → 1,2,3
;1,1,5 → 1,5,1

(define [descending? nums]
  (let ([len (length nums)])
    (if [< 1 len]
      [and [< (second nums) (first nums)] [descending? (rest nums)]]
      true)))

(define [next-permutation nums]
  (let* ([lv (add1 (apply max nums))]
         [len (length nums)]
         [pos-ws (map (lambda [p] (expt lv p)) (range (sub1 len) -1 -1))])
    (define [iter w]
      (define [available?]
        (let ([result (map (lambda [n] (remainder (floor (/ w n)) lv)) pos-ws)])
          [and [andmap = (sort result <) (sort nums <)] result]))

      [or [available?] (iter (add1 w))])
    
    (if [descending? nums]
      (reverse nums)
      (iter (add1 (apply + (map * nums pos-ws)))))))

(next-permutation '(1 2 3))
(next-permutation '(3 2 1))
(next-permutation '(1 1 5))
;(all-permutations '(1 2 2 3))
