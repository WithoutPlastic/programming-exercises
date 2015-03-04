#lang racket

;Problem:
;Given a list of non negative integers, arrange them such that they form the
;largest number.
;
;For example, given [3, 30, 34, 5, 9], the largest formed number is 9534330.
;
;Note: The result may be very large, so you need to return a string instead of
;an integer.
;
;Credits:
;Special thanks to @ts for adding this problem and creating all test cases.

(require "lib/char-number-convert.rkt")

(define [number-chars<? num-chars-a num-chars-b]
  (let ([first-char (car num-chars-a)])
    (define [iter remaining-a remaining-b]
      (match (cons [null? remaining-a] [null? remaining-b])
        ([cons #f #f] (if [eq? (car remaining-a) (car remaining-b)]
                        [iter (cdr remaining-a) (cdr remaining-b)]
                        [char<? (car remaining-a) (car remaining-b)]))
        ([cons #t #f] [or [eq? (car remaining-b) first-char]
                          [char<? first-char (car remaining-b)]])
        ([cons #f #t] [not [or [eq? (car remaining-a) first-char]
                               [char<? first-char (car remaining-a)]]])
        (_ true)))

    (iter num-chars-a num-chars-b)))

(define [largest-number nums]
  (list->string
    (apply append (sort (map number->chars nums) (negate number-chars<?)))))

(define test-nums '(3 30 34 5 9 9 9 60 60 607 602))

(largest-number test-nums)
