#lang racket

;Problem:
;Given an input string, reverse the string word by word. A word is defined as a
;sequence of non-space characters.
;
;The input string does not contain leading or trailing spaces and the words are
;always separated by a single space.
;
;For example,
;Given s = "the sky is blue",
;return "blue is sky the".
;
;Could you do it in-place without allocating extra space?

(require "reverse-words-in-a-string.rkt")

(define test-str "  the sky   is blue ")

(reverse-words test-str)
