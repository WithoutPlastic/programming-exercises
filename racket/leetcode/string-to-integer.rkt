#lang racket

;Problem:
;Implement atoi to convert a string to an integer.
;
;Hint: Carefully consider all possible input cases. If you want a challenge,
;please do not see below and ask yourself what are the possible input cases.
;
;Notes: It is intended for this problem to be specified vaguely (ie, no given
;input specs). You are responsible to gather all the input requirements up
;front.)

(define [string->integer str]
  (define max-number 2147483647)
  (define overflow-test-number 214748364)
  (define overflow-digits-char '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7))
  (define available-digits-number '(0 1 2 3 4 5 6 7 8 9))
  (define available-digits-char '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
  (define digits-table (map (lambda [c d] (cons c d))
                            available-digits-char
                            available-digits-number))
  (define [key->value k]
    (define [iter remaining]
      (cond ([null? remaining] failed-result)
            ([eq? (caar remaining) k] (cdar remaining))
            (else (iter (cdr remaining)))))
    (iter digits-table))

  (define failed-result 'failed)

  (define [iter remaining-chars accum]
    (define [continue]
      (let ([first-char (car remaining-chars)]
            [rest-chars (cdr remaining-chars)])
        (define [overflowing]
          (if [and [memq first-char overflow-digits-char] [null? rest-chars]]
            (+ (* 10 accum) (key->value first-char))
            failed-result))

        (cond ([< overflow-test-number accum] (overflowing))
              ([not [eq? (key->value first-char) failed-result]]
               (iter rest-chars (+ (* 10 accum) (key->value first-char))))
              (else failed-result))))

    (if [null? remaining-chars] accum (continue)))

  (let ([chars (string->list str)])
    (define [continue]
      (let ([first-char (car chars)]
            [rest-chars (cdr chars)])
        (cond ([and [eq? first-char #\-] [pair? rest-chars]]
               (let ([ret (iter rest-chars 0)])
                 (if [eq? ret failed-result] ret (- ret))))
              ([and [eq? first-char #\+] [pair? rest-chars]]
               (iter rest-chars 0))
              ([not [eq? (key->value first-char) failed-result]]
               (iter chars 0))
              (else failed-result))))

    (if [null? chars] failed-result (continue))))

(string->integer "1024")
(string->integer "+1024")
(string->integer "-1024")
(string->integer "+0")
(string->integer "-0")
(string->integer "+")
(string->integer "-")
(string->integer "9999999999999999999999")
(string->integer "-9999999999999999999999")
(string->integer "")
(string->integer "hello world!")
(string->integer "1024hello world!")
