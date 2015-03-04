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
  (define [iter remaining candidate cnt]
    (if [null? remaining] candidate
      (let ([first-elt (car remaining)] [rest-elts (cdr remaining)])
        (cond ([= cnt 0] (iter rest-elts first-elt (add1 cnt)))
              ([eq? first-elt candidate] (iter rest-elts candidate (add1 cnt)))
              (else (iter rest-elts candidate (sub1 cnt)))))))

  (iter lst '() 0))

(define test-lst
  (shuffle (append (build-list 50 (thunk* (random 20))) (make-list 50 7))))

(majority-element test-lst)
