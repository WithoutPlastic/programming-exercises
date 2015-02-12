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
;
;Math solution note:
;1> From right to left, find the first digit(partition number) which violate the
;increase trend, in this example, 6 will be selected since 8,7,4,3,2 already in
;a increase trend.
;
;2> From right to left, find the first digit which larger than partition number,
;call it change number. Here the 7 will be selected.
;
;3> Swap the partion number and change number.
;
;4> Reverse all the digit on the right of partition index.
;
;example>
;init   -  6  8  7  4  3  2
;step-1 - >6< 8  7  4  3  2
;step-2 -  6  8 >7< 4  3  2
;step-3 -  7  8  6  4  3  2
;step-4 -  7 <8  6  4  3  2>
;final  -  7  2  3  4  6  8

(define [find-first-descending-idx-r nums]
  (let ([len (length nums)])
    (define [iter idx]
      (cond ([= 0 idx] -1)
            ([< (list-ref nums (sub1 idx)) (list-ref nums idx)] (sub1 idx))
            (else (iter (sub1 idx)))))

    (iter (sub1 len))))

(define [swap-list-elts lst idx-s idx-l]
  (append (take lst idx-s)
          (list (list-ref lst idx-l))
          (take (drop lst (add1 idx-s)) (sub1 (- idx-l idx-s)))
          (list (list-ref lst idx-s))
          (drop lst (add1 idx-l))))

(define [next-permutation nums]
  (let ([len (length nums)]
        [sorted-non-duplicated-nums (sort (remove-duplicates nums) <)]
        [fd-idx (find-first-descending-idx-r nums)])
    (define [continue]
      (let* ([fd-num (list-ref nums fd-idx)]
             [l-num (cadr (memq fd-num sorted-non-duplicated-nums))]
             [l-idx (ormap (lambda [x] [and [= (list-ref nums x) l-num] x])
                           (range 0 len))]
             [swapped-nums (swap-list-elts nums fd-idx l-idx)])
        (append (take swapped-nums (add1 fd-idx))
                (reverse (drop swapped-nums(add1 fd-idx))))))

    (if [< fd-idx 0] (reverse nums) (continue))))

(next-permutation '(1 2 3))
(next-permutation '(3 2 1))
(next-permutation '(1 1 5))
(next-permutation '(1 3 2 2))
