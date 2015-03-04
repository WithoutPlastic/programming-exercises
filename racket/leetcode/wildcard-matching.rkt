#lang racket

;Problem:
;Implement wildcard pattern matching with support for '?' and '*'.
;
;'?' Matches any single character.
;'*' Matches any sequence of characters (including the empty sequence).
;
;The matching should cover the entire input string (not partial).
;
;The function prototype should be:
;bool isMatch(const char *s, const char *p)
;
;Some examples:
;isMatch("aa", "a") → false
;isMatch("aa", "aa") → true
;isMatch("aaa", "aa") → false
;isMatch("aa", "*") → true
;isMatch("aa", "a*") → true
;isMatch("ab", "?*") → true
;isMatch("aab", "c*a*b") → false

(define any-single-matcher #\?)
(define empty-or-any-matcher #\*)

(define [matched? str pattern]
  (let ([str-chars (string->list str)]
        [pattern-chars (string->list pattern)])
    (define [iter str-remaining pattern-remaining]
      (define [continue]
        (let ([str-first (car str-remaining)]
              [str-rest (cdr str-remaining)]
              [pattern-first (car pattern-remaining)]
              [pattern-rest (cdr pattern-remaining)])
          (define [any-sequence-matching]
            (define [walk remaining]
              (if [null? remaining]
                (iter remaining pattern-rest)
                [or (iter remaining pattern-rest)
                    (iter (cdr remaining) pattern-rest)]))

            (walk str-rest))

          (cond ([eq? pattern-first empty-or-any-matcher]
                 [or [any-sequence-matching] [iter str pattern-rest]])
                ([or [eq? pattern-first any-single-matcher]
                     [eq? pattern-first str-first]]
                 (iter str-rest pattern-rest))
                (else false))))

      (cond ([and [null? str-remaining] [null? pattern-remaining]] true)
            ([or [null? str-remaining] [null? pattern-remaining]] false)
            (else (continue))))

    (iter str-chars pattern-chars)))

(matched? "aa" "a")
(matched? "aa" "aa")
(matched? "aaa" "aa")
(matched? "aa" "*")
(matched? "aa" "a*")
(matched? "ab" "?*")
(matched? "aab" "c*a*b")
