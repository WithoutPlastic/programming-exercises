#lang racket

;Problem:
;Given a string S, find the longest palindromic substring in S. You may assume
;that the maximum length of S is 1000, and there exists one unique longest
;palindromic substring.

(define [find-longest-palindrome str]
  (define [merge-longest-list lst-a lst-b]
    (let ([len-a (length lst-a)]
          [len-b (length lst-b)])
      (if [< len-a len-b] lst-b lst-a)))

  (let* ([str-list (string->list str)]
         [len (length str-list)])
    (define [walk back-idx forward-idx accum]
      (define [prepare]
        (let ([init-char (list-ref str-list forward-idx)])
          (if [or [= back-idx 0] [= forward-idx (sub1 len)]]
            (list init-char)
            (merge-longest-list
              (walk forward-idx (add1 forward-idx) '())
              (walk (sub1 back-idx) (add1 forward-idx) (list init-char))))))

      (define [continue]
        (if [or [< back-idx 0] [= forward-idx len]]
          accum
          (let ([back-char (list-ref str-list back-idx)]
                [forward-char (list-ref str-list forward-idx)])
            (if [eq? back-char forward-char]
              (walk (sub1 back-idx)
                    (add1 forward-idx)
                    (append (list back-char) accum (list forward-char)))
              accum))))

      (if [= forward-idx back-idx] (prepare) (continue)))

    (define [iter idx]
      (if [< idx len]
        (merge-longest-list (walk idx idx '()) (iter (add1 idx)))
        '()))

    (list->string (iter 0))))

(define test-string-a "ssfhgntubsffsbuflqnvt")
(define test-string-b "ssfhgntubsfvfsbuflqnvt")
(define test-string-c "ssfhgntubsfffsbuflqnvt")

(find-longest-palindrome test-string-c)
