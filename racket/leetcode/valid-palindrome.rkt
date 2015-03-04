#lang racket

;Problem:
;Given a string, determine if it is a palindrome, considering only alphanumeric
;characters and ignoring cases.
;
;For example,
;"A man, a plan, a canal: Panama" is a palindrome.
;"race a car" is not a palindrome.
;
;Note:
;Have you consider that the string might be empty? This is a good question to
;ask during an interview.
;
;For the purpose of this problem, we define empty string as valid palindrome.

(define [palindrome? str]
  (define [lowercase-alpha? c]
    (let ([char-int (char->integer c)])
      [and [<= 97 char-int] [<= char-int 122]]))

  (let* ([pre-processed-chars (string->list (string-downcase str))]
         [alphas (filter lowercase-alpha? pre-processed-chars)])
    [equal? (reverse alphas) alphas]))

(palindrome? "")
(palindrome? "A man, a plan, a canal: Panama")
(palindrome? "race a car")
