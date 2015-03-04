#lang racket

;Problem:
;Given a string s and a dictionary of words dict, add spaces in s to construct
;a sentence where each word is a valid dictionary word.
;
;Return all such possible sentences.
;
;For example, given
;s = "catsanddog",
;dict = ["cat", "cats", "and", "sand", "dog"].
;
;A solution is ["cats and dog", "cat sand dog"].

(require "word-break.rkt")

(define [word-break str dictionary]
  (define sp #\space)

  (let ([chars-dict (map string->list dictionary)])
    (define [iter remaining result]
      (if [null? remaining] (list->string (cdr result))
        (flatten
          (map (λ [idx] (iter (drop remaining idx)
                              (append result (cons sp (take remaining idx)))))
               (filter-map (curry prefix? remaining) chars-dict)))))

    (iter (string->list str) '())))

(define test-str "catsanddog")
(define test-dict (list "cat" "cats" "and" "sand" "dog"))

(word-break test-str test-dict)
