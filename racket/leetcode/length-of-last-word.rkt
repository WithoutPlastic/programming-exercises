#lang racket

;Problem:
;Given a string s consists of upper/lower-case alphabets and empty space
;characters ' ', return the length of last word in the string.
;
;If the last word does not exist, return 0.
;
;Note: A word is defined as a character sequence consists of non-space
;characters only.
;
;For example,
;Given s = "Hello World",
;return 5.

(define [length-of-last-word str]
  (let* ([chars (string->list str)]
         [r-str (reverse chars)])
    (define [matching remaining cnt]
      (if [or [null? remaining] [eq? (car remaining) #\space]]
        cnt
        (matching (cdr remaining) (add1 cnt))))

    (define [seek remaining]
      (cond ([null? remaining] 0)
            ([not [eq? (car remaining) #\space]] (matching remaining 0))
            (else (seek (cdr remaining)))))

    (seek r-str)))

(define test-str "Hello World")

(length-of-last-word test-str)
