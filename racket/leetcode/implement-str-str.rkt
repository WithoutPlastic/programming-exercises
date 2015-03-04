#lang racket

;Problem:
;Implement strStr().
;
;Returns the index of the first occurrence of needle in haystack, or -1 if
;needle is not part of haystack.

(define [str-str str substr]
  (let* ([ht-chars (string->list str)]
         [nd-chars (string->list substr)]
         [ht-len (length ht-chars)]
         [nd-len (length nd-chars)])
    (define [iter idx]
      (define [continue]
        (define [matching offset]
          (cond ([<= nd-len offset] idx)
                ([eq? (list-ref ht-chars (+ idx offset))
                      (list-ref nd-chars offset)]
                 (matching (add1 offset)))
                (else (iter (add1 idx)))))
        
        (if [eq? (list-ref ht-chars idx) (car nd-chars)]
          (matching 1)
          (iter (add1 idx))))
      
      (if [<= idx (- ht-len nd-len)] (continue) -1))

    (if [null? nd-chars] -1 (iter 0))))

(define haystack-a "hello world")
(define haystack-b "hello world, my world!")
(define needle "world")

(str-str haystack-b needle)
