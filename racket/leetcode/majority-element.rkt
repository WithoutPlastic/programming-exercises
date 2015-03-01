#lang racket

;Problem:
;Given an array of size n, find the majority element. The majority element is
;the element that appears more than ⌊ n/2 ⌋ times.
;
;You may assume that the array is non-empty and the majority element always
;exist in the array.
;
;Credits:
;Special thanks to @ts for adding this problem and creating all test cases.

(define [majority-element lst]
  (let* ([unique-elts (remove-duplicates lst)]
         [count-lst-elt (λ [e] (cons e (count (curry equal? e) lst)))]
         [major-result (argmax cdr (map count-lst-elt unique-elts))]
         [major-elt (car major-result)]
         [major-repeat (cdr major-result)])
    (if [<= major-repeat (floor (/ (length lst) 2))] '() major-elt)))

(define test-lst
  (shuffle (append (build-list 52 (thunk* (random 20))) (make-list 48 7))))

(majority-element test-lst)
