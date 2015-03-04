#lang racket

;Problem:
;Given a string s, partition s such that every substring of the partition is a
;palindrome.
;
;Return all possible palindrome partitioning of s.
;
;For example, given s = "aab", Return:
;
;[
;["aa","b"],
;["a","a","b"]
;]

(define [my-partition str]
  (define [palindrome? lst]
    (let ([mid-idx (floor (/ (length lst) 2))])
      [equal? (reverse (take lst mid-idx)) (take-right lst mid-idx)]))

  (define [try chars]
    (define [continue len]
      (append-map
        (λ [l] (map (curry cons (take chars l)) (try (drop chars l))))
        (filter (compose palindrome? (curry take chars)) (range len 0 -1))))

    (let ([len (length chars)])
      (cond ([= len 0] (list '()))
            ([= len 1] (list (list chars)))
            (else (continue len)))))

  (try (string->list str)))

(for-each displayln (my-partition "aab"))
(for-each displayln (my-partition "aabcxxcdrezx"))
