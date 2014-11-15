#lang racket

(define [average a b] (/ (+ a b) 2))
(define fixpoint-torlerance 0.00001)
(define [close-enough? a b how-close] (< (abs (- a b)) how-close))

(define [fixed-point f init-guess]
  (define [iter cur-guess]
    (let ([next-guess (f cur-guess)])
      (if [close-enough? cur-guess next-guess fixpoint-torlerance]
        (/ (+ cur-guess next-guess) 2)
        (begin
          (display cur-guess)
          (newline)
          (iter next-guess)))))
  (iter init-guess))

(define [deriv f]
  (let ([deriv-step 0.000001])
    (lambda (x) (/ 
                  (- (f (+ x deriv-step))
                     (f x))
                  deriv-step))))

(define [average-damp f]
  (lambda (x) (average x (f x))))

(define [square x] (* x x))
(define [cube x] (* x x x))
(define [d-cube x] ((deriv cube) x))

(d-cube 5)

(define [newton-transform f]
  (lambda (x) (- x (/ (f x) ((deriv f) x)))))

(define [newton-method f init-guess]
  (fixed-point (newton-transform f) init-guess))

(define [cubic a b c]
  (lambda (x) (+ (cube x)
                 (* a (square x))
                 (* b x)
                 c)))
