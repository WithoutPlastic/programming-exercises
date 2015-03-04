#lang racket

;Problem:
;Given an array of strings, return all groups of strings that are anagrams.
;
;Note: All inputs will be in lower-case.

(define [string->anagram-hash str]
  (sort (map char->integer (string->list str)) <))

(define [anagram-hash-eq? str h] [equal? (string->anagram-hash str) h])

(define [anagrams strs]
  (define [iter remainings]
    (define [continue]
      (let* ([first-str (car remainings)]
             [first-hash (string->anagram-hash first-str)]
             [first-hash-eq? (curryr anagram-hash-eq? first-hash)])
        (append (list (filter first-hash-eq? remainings))
                (iter (filter-not first-hash-eq? remainings)))))

    (if [null? remainings] '() (continue)))

  (iter strs))

(define test-strs
  (list "abs" "sab" "hello" "hello" "ellho" "earth" "threa" "thate"))

(anagrams test-strs)
