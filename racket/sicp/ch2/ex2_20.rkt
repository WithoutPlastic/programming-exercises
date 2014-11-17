#lang racket

(define [same-parity . numbers]
  (define [single-odd-list x] (if [odd? x] (list x) (list)))
  (define [single-even-list x] (if [even? x] (list x) (list)))
  (define [iter filter-f other-numbers]
    (cond ([null? other-numbers] other-numbers)
          (else (append
                  (filter-f (car other-numbers))
                  (iter filter-f (cdr other-numbers))))))
  (cond ([odd? (car numbers)] (iter single-odd-list (cdr numbers)))
        ([even? (car numbers)] (iter single-even-list (cdr numbers)))))

(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)
