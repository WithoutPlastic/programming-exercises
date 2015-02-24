#lang racket

;Problem:
;Given a string S and a string T, find the minimum window in S which will
;contain all the characters in T in complexity O(n).
;
;For example,
;S = "ADOBECODEBANC"
;T = "ABC"
;
;Minimum window is "BANC".
;
;Note:
;If there is no such window in S that covers all characters in T, return the
;emtpy string "".
;
;If there are multiple such windows, you are guaranteed that there will always
;be only one unique minimum window in S.

(define [min-window str-s str-t]
  (let* ([chars-s (string->list str-s)]
         [chars-t (remove-duplicates (string->list str-t))]
         [t-char? (curryr memq chars-t)]
         [not-t-char? (negate t-char?)]
         [all-t-chars-included? (λ [lst] [andmap (curryr memq lst) chars-t])])
    (define [walk remaining-s accum result]
      (define [continue]
        (let* ([first-char (car remaining-s)] [rest-chars (cdr remaining-s)]
               [extended-accum (append accum (list first-char))])
          (define [merge-result]
            (cond ([null? result] extended-accum)
                  ([< (length extended-accum) (length result)] extended-accum)
                  (else result)))

          (cond ([and [not-t-char? first-char] [null? accum]]
                 (walk rest-chars accum result))
                ([and [not-t-char? first-char] [not [null? accum]]]
                 (walk rest-chars extended-accum result))
                ([memq first-char accum]
                 (walk rest-chars
                       (dropf extended-accum
                              (λ [x] [or [not-t-char? x] [eq? x first-char]]))
                       result))
                ([all-t-chars-included? extended-accum]
                 (walk rest-chars
                       (dropf (cdr extended-accum) not-t-char?)
                       (merge-result)))
                (else (walk rest-chars extended-accum result)))))

      (if [null? remaining-s] result (continue)))

    (list->string (walk chars-s '() '()))))

(define test-str-s "ADOBECODEBANC")
(define test-str-t "ABC")

(min-window test-str-s test-str-t)
