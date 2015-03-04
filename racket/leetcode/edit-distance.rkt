#lang racket

;Problem:
;Given two words word1 and word2, find the minimum number of steps required to
;convert word1 to word2. (each operation is counted as 1 step.)
;
;You have the following 3 operations permitted on a word:
;
;a) Insert a character
;b) Delete a character
;c) Replace a character

(define [drop-elt lst idx] (append (take lst idx) (drop lst (add1 idx))))

(define [replace-distance chars-a chars-b]
  (define [iter remaining-a remaining-b]
    (define [continue]
      (let ([f-char-a (car remaining-a)] [rest-a (cdr remaining-a)]
            [f-char-b (car remaining-b)] [rest-b (cdr remaining-b)])
        (if [eq? f-char-a f-char-b]
          (iter rest-a rest-b)
          (add1 (iter rest-a rest-b)))))

    (if [or [null? remaining-a] [null? remaining-b]] 0 (continue)))

  (iter chars-a chars-b))

(define [min-distance str-a str-b]
  (let* ([chars-a (string->list str-a)]
         [chars-b (string->list str-b)]
         [len-a (length chars-a)] [len-b (length chars-b)]
         [s-len (min len-a len-b)] [l-len (max len-a len-b)]
         [l-chars (if [< len-a len-b] chars-b chars-a)]
         [s-chars (if [< len-a len-b] chars-a chars-b)]
         [drop-cnt (abs (- len-a len-b))])
    (define [drop-iter result cnt]
      (define [continue]
        (drop-iter (append-map (lambda [ret] (map (curry drop-elt ret)
                                                  (range 0 (+ s-len cnt))))
                               result)
                   (sub1 cnt)))

      (if [< 0 cnt] (continue) result))

    (+ drop-cnt (apply min (map (curry replace-distance s-chars)
                                (drop-iter (list l-chars) drop-cnt))))))

(define test-word-a "hello world")
(define test-word-b "worrd")

(min-distance test-word-a test-word-b)
