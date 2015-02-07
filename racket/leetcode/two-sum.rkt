#lang racket

;Problem: two-sum
;Given an array of integers, find two numbers such that they add up to a
;specific target number.
;
;The function twoSum should return indices of the two numbers such that they add
;up to the target, where index1 must be less than index2. Please note that your
;returned answers (both index1 and index2) are not zero-based.
;
;You may assume that each input would have exactly one solution.
;You may assume that array of integers is ascending
;
;Input: numbers={2, 7, 11, 15}, target=9
;Output: index1=1, index2=2

(define [print-result idx-a idx-b]
  (display "index1=")
  (display idx-a)
  (display ", index2=")
  (display idx-b)
  (newline))

(define [two-sum integers target-sum]
  (define [iter small-int s-idx available-ints]
    (define [walk remaining-ints r-idx]
      (unless [null? remaining-ints]
        ;remove when expression when numbers array not ascending
        (when [<= (+ small-int (car remaining-ints)) target-sum]
          (if [= (+ small-int (car remaining-ints)) target-sum]
            (print-result s-idx (+ s-idx r-idx))
            (walk (cdr remaining-ints) (add1 r-idx))))))

    (unless [null? available-ints]
      ;remove when expression when numbers array not ascending
      (when [<= (+ small-int (car available-ints)) target-sum]
        (walk available-ints 1)
        ;remove below if have exactly one solution
        (iter (car available-ints) (add1 s-idx) (cdr available-ints)))))

  (iter (car integers) 1 (cdr integers)))

(define numbers '(2 7 11 15))
;(define numbers '(1 2 3 4 5 6 7 8 9))
(define target 9)

(two-sum numbers target)
