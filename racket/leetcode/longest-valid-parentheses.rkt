#lang racket

;Problem:
;Given a string containing just the characters '(' and ')', find the length of
;the longest valid (well-formed) parentheses substring.
;
;For "(()", the longest valid parentheses substring is "()", which has length =
;2.
;
;Another example is ")()())", where the longest valid parentheses substring is
;"()()", which has length = 4.

(define op-parenthesis #\()
(define ed-parenthesis #\))

(define [longest-valid-parentheses str]
  (let ([chars (string->list str)])
    (define [iter remaining cnt a-cnt max-len]
      (let ([next-a-cnt (add1 a-cnt)]
            [cur-len (max (- a-cnt cnt) max-len)])
        (define [continue]
          (let ([f-char (car remaining)]
                [rst (cdr remaining)])
            (cond ([and [= cnt 0] [eq? f-char ed-parenthesis]]
                   (iter rst 0 0 cur-len))
                  ([eq? f-char ed-parenthesis]
                   (iter rst (sub1 cnt) next-a-cnt max-len))
                  (else (iter rst (add1 cnt) next-a-cnt max-len)))))

        (cond ([null? remaining] cur-len)
              ([< max-len (+ (length remaining) a-cnt)] (continue))
              (else max-len))))

    (iter chars 0 0 0)))

(define test-str-a "(()")
(define test-str-b ")(()))")
(define test-str-c ")((((()()()))))()((())(()))()()()()()()))))")

(longest-valid-parentheses test-str-a)
(longest-valid-parentheses test-str-b)
(longest-valid-parentheses test-str-c)
