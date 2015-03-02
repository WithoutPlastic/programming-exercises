#lang racket

;Problem:
;Given a string s, partition s such that every substring of the partition is a
;palindrome.
;
;Return the minimum cuts needed for a palindrome partitioning of s.
;
;For example, given s = "aab",
;Return 1 since the palindrome partitioning ["aa","b"] could be produced using
;1 cut.

(define [min-cut str]
  (define [palindrome? lst]
    (let ([mid-idx (floor (/ (length lst) 2))])
      [equal? (reverse (take lst mid-idx)) (take-right lst mid-idx)]))

  (define [try chars]
    (let ([len (length chars)])
      (if [or [= len 0] [= len 1]] 0
        (add1 (try (drop chars (findf (λ [l] [palindrome? (take chars l)])
                                      (range len 0 -1))))))))

  (try (string->list str)))

(min-cut "aab")
(min-cut "aabcxxcdrezxxz")
