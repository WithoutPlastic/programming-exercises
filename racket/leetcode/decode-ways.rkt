#lang racket

;Problem:
;A message containing letters from A-Z is being encoded to numbers using the
;following mapping:
;
;'A' -> 1
;'B' -> 2
;...
;'Z' -> 26
;
;Given an encoded message containing digits, determine the total number of ways
;to decode it.
;
;For example,
;Given encoded message "12", it could be decoded as "AB" (1 2) or "L" (12).
;
;The number of ways decoding "12" is 2.

(require "lib/char-number-convert.rkt")

(define max-num 26)
(define [gen-decode-table]
  (let ([decode-num (range 1 (add1 max-num))]
        [alphabets (string->list "ABCDEFGHIJKLMNOPQRSTUVWXYZ")])
    (map cons decode-num alphabets)))
(define decode-table (gen-decode-table))

(define one #\1) (define two #\2)
(define [first-char-valid? c] [or [eq? c one] [eq? c two]])

(define [num-decodings str]
  (define [iter remaining]
    (define [continue]
      (let ([first-char (car remaining)]
            [dropped-one-chars (cdr remaining)]
            [dropped-two-chars (drop remaining 2)]
            [full-num (chars->number (take remaining 2))])
        (if [first-char-valid? first-char]
          (if [< max-num full-num]
            (iter dropped-one-chars)
            (+ (iter dropped-one-chars) (iter dropped-two-chars)))
          (iter dropped-one-chars))))

    (let ([len (length remaining)])
      (if [or [= len 0] [= len 1]] 1 (continue))))

  (let ([chars (string->list str)])
    (if [null? chars] 0 (iter chars))))

(define test-input "1223461245687")

(num-decodings test-input)
