#lang racket

;Problem:
;Given a non-negative number represented as an array of digits, plus one to the
;number.
;
;The digits are stored such that the most significant digit is at the head of
;the list.

(define zero #\0) (define one #\1) (define nine #\9)
(define zero-eq? (curry eq? zero))
(define one-eq? (curry eq? one))
(define nine-eq? (curry eq? nine))
(define [nchar-plus-one c] (integer->char (add1 (char->integer c))))

(define [plus-one num-str]
  (let* ([num-chars (string->list num-str)]
         [len (length num-chars)])
    (define [iter idx c]
      (define [continue]
        (let ([cur-char (list-ref num-chars idx)])
          (cond ([and [one-eq? c] [nine-eq? cur-char]]
                 (append (iter (sub1 idx) one) (list zero)))
                ([one-eq? c]
                 (append (take num-chars idx) (list (nchar-plus-one cur-char))))
                (else (take num-chars (add1 idx))))))

      (cond ([and [< idx 0] [zero-eq? c]] '())
            ([< idx 0] (list c))
            (else (continue))))

    (iter (sub1 len) one)))

(define test-number-a "123456789")
(define test-number-b "999999999")

(plus-one test-number-a)
(plus-one test-number-b)
