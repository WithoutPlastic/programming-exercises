#lang racket

;Problem:
;Given an array with n objects colored red, white or blue, sort them so that
;objects of the same color are adjacent, with the colors in the order red,
;white and blue.
;
;Here, we will use the integers 0, 1, and 2 to represent the color red, white,
;and blue respectively.
;
;Note: You are not suppose to use the library's sort function for this problem.

(define r #\r) (define w #\w) (define b #\b)
(define r? (curry eq? r)) (define w? (curry eq? w)) (define b? (curry eq? b))

(define [sort-colors lst]
  (define [iter remaining r-cnt w-cnt b-cnt]
    (define [continue]
      (let ([f-elt (car remaining)]
            [rest-elts (cdr remaining)])
        (cond ([r? f-elt] (iter rest-elts (add1 r-cnt) w-cnt b-cnt))
              ([w? f-elt] (iter rest-elts r-cnt (add1 w-cnt) b-cnt))
              (else (iter rest-elts r-cnt w-cnt (add1 b-cnt))))))

    (if [null? remaining]
      (append (make-list r-cnt r) (make-list w-cnt w) (make-list b-cnt b))
      (continue)))

  (iter lst 0 0 0))

(define test-color-list (list w w b b r r r r b r b r b w b w b r r w w))

(sort-colors test-color-list)
