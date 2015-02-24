#lang racket

;Problem:
;Given n non-negative integers representing the histogram's bar height where
;the width of each bar is 1, find the area of largest rectangle in the
;histogram.
;
;Above is a histogram where width of each bar is 1, given height =
;[2,1,5,6,2,3].
;
;The largest rectangle is shown in the shaded area, which has area = 10 unit.
;
;For example,
;Given height = [2,1,5,6,2,3],
;return 10.

(define [largest-rectangle-area levels]
  (let ([sorted-unique-lvls (sort (remove-duplicates levels) <)])
    (define [scan-area lvl]
      (let* ([idxs (range 0 (length levels))]
             [valid-idxs (filter (λ [i] [<= lvl (list-ref levels i)]) idxs)])
        (define [max-len remaining cur-result max-result]
          (let ([first-idx (car remaining)] [second-idx (cadr remaining)]
                [rest-idxs (cdr remaining)])
            (cond ([null? rest-idxs] (max cur-result max-result))
                  ([= (add1 first-idx) second-idx]
                   (max-len rest-idxs (add1 cur-result) max-result))
                  (else (max-len rest-idxs 1 (max cur-result max-result))))))

        (* (max-len valid-idxs 1 1) lvl)))

    (define [iter remaining-lvls max-area]
      (define [continue]
        (let ([cur-area (scan-area (car remaining-lvls))]
              [rest-lvls (cdr remaining-lvls)])
          (if [< cur-area max-area]
            (iter rest-lvls max-area)
            (iter rest-lvls cur-area))))

      (if [null? remaining-lvls] max-area (continue)))

    (iter sorted-unique-lvls 0)))

(define test-heights '(2 1 5 6 2 3))

(largest-rectangle-area test-heights)
