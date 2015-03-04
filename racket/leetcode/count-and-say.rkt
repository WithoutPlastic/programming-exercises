#lang racket

;Problem:
;The count-and-say sequence is the sequence of integers beginning as follows:
;1, 11, 21, 1211, 111221, ...
;
;1 is read off as "one 1" or 11.
;11 is read off as "two 1s" or 21.
;21 is read off as "one 2, then one 1" or 1211.
;Given an integer n, generate the nth sequence.
;
;Note: The sequence of integers will be represented as a string.

(require "lib/char-number-convert.rkt")

(define [count-and-say str]
  (define [next chars]
    (define [walk remainings cur-char repeat]
      (let ([cur-slice (list (number->char repeat) cur-char)])
        (define [continue]
          (let ([f-char (car remainings)] [rest-chars (cdr remainings)])
            (if [eq? f-char cur-char]
              (walk rest-chars cur-char (add1 repeat))
              (append cur-slice (walk rest-chars f-char 1)))))

        (if [null? remainings] cur-slice (continue))))

    (walk (cdr chars) (car chars) 1))

  (define [iter input cnt]
    (when [< 0 cnt]
      (let ([next-chars (next input)])
        (displayln next-chars)
        (iter next-chars (sub1 cnt)))))

  (let ([repeat (string->number str)] [chars (string->list str)])
    (iter chars repeat)))

;(define test-input-a "1")
(define test-input-b "11")

;(count-and-say test-input-a)
(count-and-say test-input-b)
