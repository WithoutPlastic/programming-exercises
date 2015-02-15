#lang racket

;Problem:
;Given two numbers represented as strings, return multiplication of the numbers
;as a string.
;
;Note: The numbers can be arbitrarily large and are non-negative.

(define [char->number c] (- (char->integer c) 48))
(define [number->char i] (integer->char (+ i 48)))

(define [cin multipled-nums]
  (define [cin ns]
    (define [continue]
      (let* ([first-num (car ns)]
             [second-num (cadr ns)]
             [rest-nums (cddr ns)]
             [r (remainder first-num 10)]
             [c (floor (/ first-num 10))])
        (cons r (cin (cons (+ second-num c) rest-nums)))))

    (if [null? (cdr ns)] ns (continue)))

  (let ([w/-padding (cons 0 multipled-nums)])
    (reverse (cin (reverse w/-padding)))))

(define [char-multiply-chars n-char n-chars]
  (let ([num (char->number n-char)]
        [nums (map char->number n-chars)])
    (map (curry * num) nums)))

(define [multiply factor-str str]
  (let* ([factor-chars (string->list factor-str)]
         [factor-len (length factor-chars)]
         [chars (string->list str)]
         [nums-list (map (curryr char-multiply-chars chars) factor-chars)]
         [add-padding (map (lambda [x n]
                             (append (make-list n 0)
                                     x
                                     (make-list (- (sub1 factor-len) n) 0)))
                           nums-list
                           (range 0 factor-len))]
         [added-nums (apply map + add-padding)]
         [cin-nums (cin added-nums)])
    (list->string
      (map number->char (if [= (car cin-nums) 0] (cdr cin-nums) cin-nums)))))

(define test-str-a-a "4321")
(define test-str-b-a "1234")
(define test-str-a-b "987")
(define test-str-b-b "9876543210")

(multiply test-str-a-a test-str-b-a)
(multiply test-str-a-b test-str-b-b)
(multiply test-str-b-b test-str-a-b)
