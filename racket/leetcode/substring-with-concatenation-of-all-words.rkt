#lang racket

;Problem:
;You are given a string, S, and a list of words, L, that are all of the same
;length. Find all starting indices of substring(s) in S that is a concatenation
;of each word in L exactly once and without any intervening characters.
;
;For example, given:
;S: "barfoothefoobarman"
;L: ["foo", "bar"]
;
;You should return the indices: [0,9] (order does not matter).

(define [all-permutations elts]
  (cond ([null? elts] '())
        ([= 1 (length elts)] (list elts))
        (else (apply append
                     (map (lambda [elt]
                            (map (lambda [x] (cons elt x))
                                 (all-permutations
                                   (filter-not (lambda [x] [eq? elt x]) elts))))
                          elts)))))

(define [find-sub-string str substrs]
  (let* ([combinations (all-permutations substrs)]
         [concatenations (map (lambda [x] (foldl string-append "" x))
                              combinations)]
         [str-chars (string->list str)]
         [concat-chars-list (map string->list concatenations)])
    (define [str-str nd-chars]
      (let* ([ht-len (length str-chars)]
             [nd-len (length nd-chars)])
        (define [iter idx]
          (define [continue]
            (define [matching offset]
              (cond ([<= nd-len offset] idx)
                    ([eq? (list-ref str-chars (+ idx offset))
                          (list-ref nd-chars offset)]
                     (matching (add1 offset)))
                    (else (iter (add1 idx)))))

            (if [eq? (list-ref str-chars idx) (car nd-chars)]
              (matching 1)
              (iter (add1 idx))))

          (if [<= idx (- ht-len nd-len)] (continue) -1))

        (if [null? nd-chars] -1 (iter 0))))

    (filter-not (lambda [x] [= x -1])
                (map (lambda [sub] (str-str sub)) concat-chars-list))))

(define test-string-a "barfoothefoobarman")
(define substrings-a (list "foo" "bar"))
(define test-string-b "barfoothefootbarfootman")
(define substrings-b (list "foo" "bar" "t"))

(find-sub-string test-string-a substrings-a)
(find-sub-string test-string-b substrings-b)
