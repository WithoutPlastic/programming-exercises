#lang racket

;Problem:
;Evaluate the value of an arithmetic expression in Reverse Polish Notation.
;
;Valid operators are +, -, *, /. Each operand may be an integer or another
;expression.
;
;Some examples:
;
;["2", "1", "+", "3", "*"] -> ((2 + 1) * 3) -> 9
;["4", "13", "5", "/", "+"] -> (4 + (13 / 5)) -> 6

(require "lib/char-number-convert.rkt")

(define operator-table
  (list (cons #\+ +) (cons #\- -) (cons #\* *) (cons #\/ /)))

(define [eval-rpn tokens]
  (define [walk remaining accum]
    (define [not-operator? elt] [not (assoc (car elt) operator-table)])
    (define [calc op op-a op-b]
      (let ([int-a (chars->number op-a)] [int-b (chars->number op-b)]
            [operator (cdr (assoc (car op) operator-table))])
        (number->chars (operator int-a int-b))))

    (if [null? remaining] (list->string (car accum))
      (let ([first-elt (car remaining)] [rest-elts (cdr remaining)])
        (if [not-operator? first-elt]
          (walk rest-elts (cons first-elt accum))
          (let ([op-a (car accum)] [op-b (cadr accum)])
            (walk rest-elts (cons (calc first-elt op-a op-b) (cddr accum))))))))

  (walk (map string->list tokens) '()))

(define test-tokens-a '("2" "1" "+" "3" "*"))
(define test-tokens-b '("4" "13" "5" "/" "+"))

(eval-rpn test-tokens-a)
(eval-rpn test-tokens-b)
