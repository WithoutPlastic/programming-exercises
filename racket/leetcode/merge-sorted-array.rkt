#lang racket

;Problem:
;Given two sorted integer arrays A and B, merge B into A as one sorted array.
;
;Note:
;You may assume that A has enough space (size that is greater or equal to m + n)
;to hold additional elements from B. The number of elements initialized in A and
;B are m and n respectively.

(define [merge ints-a ints-b]
  (define [iter remaining-a remaining-b]
    (define [continue]
      (let ([f-a (car remaining-a)] [r-a (cdr remaining-a)]
            [f-b (car remaining-b)] [r-b (cdr remaining-b)])
        (if [< f-a f-b]
          (cons f-a (iter r-a remaining-b))
          (cons f-b (iter remaining-a r-b)))))

    (cond ([null? remaining-a] remaining-b)
          ([null? remaining-b] remaining-a)
          (else (continue))))

  (iter ints-a ints-b))

(define test-ints-a '(-9 -7 -4 1 3 5 6 6 6 10))
(define test-ints-b '(-10 -2 -2 -1 -1 0 0 5 5 8))

(merge test-ints-a test-ints-b)
