#lang racket

;Problem:
;Given s1, s2, s3, find whether s3 is formed by the interleaving of s1 and s2.
;
;For example,
;Given:
;s1 = "aabcc",
;s2 = "dbbca",
;
;When s3 = "aadbbcbcac", return true.
;When s3 = "aadbbbaccc", return false.

(define [interleaving? sub-str-a sub-str-b result-str]
  (let* ([sub-chars-a (string->list sub-str-a)] [len-a (length sub-chars-a)]
         [sub-chars-b (string->list sub-str-b)] [len-b (length sub-chars-b)]
         [result-chars (string->list result-str)] [len-r (length result-chars)])
    (define [iter remaining-a remaining-b remaining-result]
      (define [continue]
        (let ([first-a (car remaining-a)] [rest-a (cdr remaining-a)]
              [first-b (car remaining-b)] [rest-b (cdr remaining-b)]
              [first-r (car remaining-result)] [rest-r (cdr remaining-result)])
          (cond ([and [eq? first-r first-a] [eq? first-r first-b]]
                 [or (iter rest-a remaining-b rest-r)
                     (iter remaining-a rest-b rest-r)])
                ([eq? first-r first-a] (iter rest-a remaining-b rest-r))
                ([eq? first-r first-b] (iter remaining-a rest-b rest-r))
                (else false))))

      (cond ([null? remaining-result] true)
            ([null? remaining-a] [equal? remaining-b remaining-result])
            ([null? remaining-b] [equal? remaining-a remaining-result])
            (else (continue))))

    (if [= (+ len-a len-b) len-r]
      (iter sub-chars-a sub-chars-b result-chars)
      false)))

(define test-str-a "aabcc")
(define test-str-b "dbbca")
(define test-str-c-t "aadbbcbcac")
(define test-str-c-f "aadbbbaccc")

(interleaving? test-str-a test-str-b test-str-c-t)
(interleaving? test-str-a test-str-b test-str-c-f)
