#lang racket

;Problem:
;Given a string s and a dictionary of words dict, determine if s can be
;segmented into a space-separated sequence of one or more dictionary words.
;
;For example, given
;s = "leetcode",
;dict = ["leet", "code"].
;
;Return true because "leetcode" can be segmented as "leet code".

(define [prefix? chars-s chars-p]
  (let ([len-s (length chars-s)] [len-p (length chars-p)])
    (define [iter idx]
      (match (list [< idx len-s] [< idx len-p])
        ([list _ #f] idx)
        ([list #f #t] false)
        ([list #t #t]
         [and [eq? (list-ref chars-s idx) (list-ref chars-p idx)]
              (iter (add1 idx))])))

    (iter 0)))
(provide prefix?)

(define [word-break str dictionary]
  (let ([chars-dict (map string->list dictionary)])
    (define [iter remaining]
      [or [null? remaining]
          [ormap (compose iter (curry drop remaining))
                 (filter-map (curry prefix? remaining) chars-dict)]])

    (iter (string->list str))))

;(define test-str-a "leetcodelcodeleetcode")
;(define test-str-b "leetcodelcodeleetcodeleetl")
;(define test-dict (list "leet" "code" "codel"))
;
;(word-break test-str-a test-dict)
;(word-break test-str-b test-dict)
