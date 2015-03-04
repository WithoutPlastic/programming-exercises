#lang racket

;Problem:
;Given an array and a value, remove all instances of that value in place and
;return the new length.
;
;The order of elements can be changed. It doesn't matter what you leave beyond
;the new length.

(define [remove-element boxed-list num]
  (let* ([nums (unbox boxed-list)])
    (define [iter remainings]
      (cond ([null? remainings] (list num))
            ([= (car remainings) num] (iter (cdr remainings)))
            (else (cons (car remainings) (iter (cdr remainings))))))

    (set-box! boxed-list (iter nums))
    (length (unbox boxed-list))))

(define boxed-test-nums (box '(1 2 3 4 -1 -1 2 2 9 9 10 3 -3)))

(remove-element boxed-test-nums 2)
boxed-test-nums
