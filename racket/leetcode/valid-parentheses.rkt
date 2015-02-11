#lang racket

;Problem:
;Given a string containing just the characters '(', ')', '{', '}', '[' and ']',
;determine if the input string is valid.
;
;The brackets must close in the correct order, "()" and "()[]{}" are all valid
;but "(]" and "([)]" are not.

(define brackets (list (cons #\( #\)) (cons #\[ #\]) (cons #\{ #\})))
(define [enter-char? char] [memq char (map car brackets)])
(define [get-exit-char char] (cdr (assq char brackets)))

(define [validate-parentheses input-string]
  (let ([chars (string->list input-string)])
    (define [iter remainings exit-proc]
      (define [continue]
        (let ([f-char (car remainings)]
              [rest-chars (cdr remainings)])
          (define [enter char]
            (define [exit r]
              (cond ([null? r] false)
                    ([enter-char? (car r)] (iter r exit))
                    ([eq? (car r) char] (exit-proc (cdr r)))
                    (else false)))
            (iter rest-chars exit))

          (if [enter-char? f-char]
            (enter (get-exit-char f-char))
            (exit-proc remainings))))

      (if [null? remainings] (exit-proc remainings) (continue)))

    (iter chars (const true))))

(define valid-string "{[]({[[[()]]]})}")
(define invalid-string "{[]({[[[()]]]}){}")

(validate-parentheses valid-string)
(validate-parentheses invalid-string)
