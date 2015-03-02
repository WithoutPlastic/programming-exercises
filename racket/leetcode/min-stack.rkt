#lang racket

;Problem:
;Design a stack that supports push, pop, top, and retrieving the minimum
;element in constant time.
;
; - push(x) -- Push element x onto stack.
; - pop() -- Removes the element on top of the stack.
; - top() -- Get the top element.
; - getMin() -- Retrieve the minimum element in the stack.

(define stack (new (class object%
  (super-new)
  (define stack '())
  (define min-val +inf.0)
  (define/public [push! elt]
    (set! stack (cons elt stack))
    (when [< elt min-val] (set! min-val elt)))
  (define/public [pop!]
    (if [null? stack] '()
      (let ([elt (car stack)])
        (set! stack (cdr stack))
        (when [= elt min-val] (set! min-val (apply min stack)))
        elt)))
  (define/public [top] (car stack))
  (define/public [get-min] min-val))))

(send stack pop!)
(send stack push! 1)
(send stack push! 2)
(send stack push! 0)
(send stack push! 1)
(send stack push! 2)
(send stack push! 3)
(send stack pop!)
(send stack push! 4)
(send stack push! 5)
(send stack pop!)
(send stack top)
(send stack get-min)
