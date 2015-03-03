#lang racket

;Problem:
;Given a string S and a string T, count the number of distinct subsequences of
;T in S.
;
;A subsequence of a string is a new string which is formed from the original
;string by deleting some (can be none) of the characters without disturbing the
;relative positions of the remaining characters. (ie, "ACE" is a subsequence of
;"ABCDE" while "AEC" is not).
;
;Here is an example:
;
;S = "rabbbit", T = "rabbit"
;Return 3.

(require "lib/permutation.rkt")

(define [distincts str-s str-t]
  (let ([len-s (string-length str-s)] [len-t (string-length str-t)])
    (cond ([= len-s len-t] (if [equal? str-s str-t] 1 0))
          ([< len-s len-t] (distincts str-t str-s))
          (else (count (curry equal? (string->list str-t))
                       (drop-n-elts (string->list str-s) (- len-s len-t)))))))

(distincts "rabbbbbit" "rabbit")
