#lang racket

;Problem:
;Given a digit string, return all possible letter combinations that the number
;could represent.
;
;A mapping of digit to letters (just like on the telephone buttons) is given
;below.
;
;Input: Digit string "23"
;Output: ["ad", "ae", "af", "bd", "be", "bf", "cd", "ce", "cf"].
;
;Note: Although the above answer is in lexicographical order, your answer could
;be in any order you want.

(define phone-number-letter-table (make-hash))
(hash-set*! phone-number-letter-table
            #\0 (list #\space)
            #\1 (list #\?)
            #\2 (list #\a #\b #\c)
            #\3 (list #\d #\e #\f)
            #\4 (list #\g #\h #\i)
            #\5 (list #\j #\k #\l)
            #\6 (list #\m #\n #\o)
            #\7 (list #\p #\q #\r #\s)
            #\8 (list #\t #\u #\v)
            #\9 (list #\w #\x #\y #\z)
            #\* (list #\+)
            #\# (list #\A))

(define [phone-letter-combinations number-string]
  (let* ([number-chars (string->list number-string)]
         [ref (lambda [x] (hash-ref phone-number-letter-table x))]
         [number-letters (map ref number-chars)])
    (define [iter remainings]
      (if [null? remainings]
        (list '())
        (foldr append '()
               (map (lambda [x] (map (lambda [y] (cons y x)) (car remainings)))
                    (iter (cdr remainings))))))

    (map list->string (iter number-letters))))

(define test-input "234")

(phone-letter-combinations test-input)
