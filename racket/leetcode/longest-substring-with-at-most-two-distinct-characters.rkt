#lang racket

;Problem:
;Given a string S, find the length of the longest substring T that contains at
;most two distinct characters.
;
;For example,
;
;Given S = “eceba”,
;T is “ece” which its length is 3.

(define [length-of-longest-substring-two-distinct str n]
  (define [iter remaining accum]
    (if [null? remaining] (length accum)
      (let ([first-c (car remaining)]
            [rests (cdr remaining)])
        (if [or [memq first-c accum] [< (length (remove-duplicates accum)) n]]
          (iter rests (append accum (list first-c)))
          (length accum)))))

  (define [split remaining]
    (if [<= (length remaining) n] n
      (let ([first-ret (iter remaining '())]
            [rest-ret (split (cdr remaining))])
        (if [< first-ret rest-ret] rest-ret first-ret))))

  (split (string->list str)))

(length-of-longest-substring-two-distinct "eceeeeccceba" 2)
