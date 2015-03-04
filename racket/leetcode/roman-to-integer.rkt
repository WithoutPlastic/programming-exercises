#lang racket

;Problem:
;Given a roman numeral, convert it to an integer.
;
;Input is guaranteed to be within the range from 1 to 3999.

(define [roman->integer str]
  (define roman-char-numer-table
    (list (list #\M #\? #\? 1000)
          (list #\D #\? #\? 500)
          (list #\C #\D #\M 100)
          (list #\L #\? #\? 50)
          (list #\X #\L #\C 10)
          (list #\V #\? #\? 5)
          (list #\I #\V #\X 1)))
  (define [from-char-number-table char select]
    (define [iter remainings]
      (cond ([null? remainings]
             (error "illegal roman char -- GET-CHAR-NUMBER"))
            ([eq? (caar remainings) char] (select (car remainings)))
            (else (iter (cdr remainings)))))
    (iter roman-char-numer-table))
  (define [get-lhf-char char] (from-char-number-table char cadr))
  (define [get-llv-char char] (from-char-number-table char caddr))
  (define [get-char-number char] (from-char-number-table char cadddr))

  (define [get-total remainings]
    (if [null? remainings]
      0
      (+ (get-char-number (car remainings))
         (get-total (cdr remainings)))))

  (define [get-minus remainings]
    (define [iter]
      (let* ([first-char (car remainings)]
             [second-char (cadr remainings)] 
             [exp-second-lhf-char (get-lhf-char first-char)]
             [exp-second-llv-char (get-llv-char first-char)])
        (if [or [eq? second-char exp-second-lhf-char]
                [eq? second-char exp-second-llv-char]]
          (+ (* 2 (get-char-number first-char))
             (get-minus (cddr remainings)))
          (get-minus (cdr remainings)))))
    (if [< 1 (length remainings)] (iter) 0))

  (let ([chars (string->list str)])
    (- (get-total chars)
       (get-minus chars))))

(roman->integer "")
(roman->integer "I")
(roman->integer "II")
(roman->integer "III")
(roman->integer "IV")
(roman->integer "V")
(roman->integer "VI")
(roman->integer "VII")
(roman->integer "VIII")
(roman->integer "IX")
(roman->integer "X")
(roman->integer "XV")
(roman->integer "XXIII")
(roman->integer "XXXIX")
(roman->integer "XLIX")
(roman->integer "L")
(roman->integer "LXXXIII")
(roman->integer "XC")
(roman->integer "C")
(roman->integer "D")
(roman->integer "DL")
(roman->integer "DCCVII")
(roman->integer "DCCCXC")
(roman->integer "MD")
(roman->integer "MDCCC")
(roman->integer "CM")
(roman->integer "MMMCMXCIX")
