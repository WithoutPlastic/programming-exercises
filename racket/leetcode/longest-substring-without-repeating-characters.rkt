#lang racket

;Problem:
;Given a string, find the length of the longest substring without repeating
;characters.
;
;For example
;the longest substring without repeating letters for "abcabcbb" is "abc", which
;the length is 3. For "bbbbb" the longest substring is "b", with the length of
;1.

(define [length-of-longest-substring str]
  (define [matching chars]
    (define [walk to-be-walk-chars walked-chars idx]
      (define [continue]
        (let* ([met-char (car to-be-walk-chars)]
               [rest-char (cdr to-be-walk-chars)]
               [cur-walked-char (append walked-chars (list met-char))])
          (cond ([and [eq? met-char (list-ref walked-chars idx)]
                      [= idx 1] [eq? met-char (car walked-chars)]]
                 (walk rest-char cur-walked-char 0))
                ([eq? met-char (list-ref walked-chars idx)]
                 (walk rest-char cur-walked-char (add1 idx)))
                (else (max idx (walk rest-char cur-walked-char 0))))))
      
      (if [null? to-be-walk-chars] idx (continue)))
    
    (walk (cdr chars) (list (car chars)) 0))

  (define [iter remaining-chars]
    (if [null? remaining-chars]
      0
      (max (matching remaining-chars)
           (iter (cdr remaining-chars)))))

  (iter (string->list str)))

(define test-string-a "ffff")
(define test-string-b "abcdabcd")

(length-of-longest-substring test-string-b)
