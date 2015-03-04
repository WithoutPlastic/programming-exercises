#lang racket

;Problem:
;Validate if a given string is numeric.
;
;Some examples:
;"0"     => true
;" 0.1 " => true
;"abc"   => false
;"1 a"   => false
;"2e10"  => true
;
;Note: It is intended for the problem statement to be ambiguous. You should
;gather all requirements up front before implementing one.

(define dot? (curry eq? #\.)) (define e? (curry eq? #\e))
(define [num? c]
  (let ([zero-int (char->integer #\0)] [nine-int (char->integer #\9)]
        [char-int (char->integer c)])
    [and [<= zero-int char-int] [<= char-int nine-int]]))

(define [number? str]
  (let ([chars (string->list (string-trim str))])
    (cond ([null? chars] false)
          ([and [num? (first chars)] [num? (last chars)]]
           (andmap (lambda [x] [or [num? x] [dot? x] [e? x]]) chars))
          (else false))))

(define test-str-a "0")
(define test-str-b " 0.1 ")
(define test-str-c "abc")
(define test-str-d "1 a")
(define test-str-e "2e10")

(number? test-str-a)
(number? test-str-b)
(number? test-str-c)
(number? test-str-d)
(number? test-str-e)
