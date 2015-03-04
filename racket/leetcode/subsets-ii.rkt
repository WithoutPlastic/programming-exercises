#lang racket

;Problem:
;Given a collection of integers that might contain duplicates, S, return all
;possible subsets.
;
;Note:
;Elements in a subset must be in non-descending order.
;The solution set must not contain duplicate subsets.
;
;For example,
;If S = [1,2,2], a solution is:
;
;[
;[2],
;[1],
;[1,2,2],
;[2,2],
;[1,2],
;[]
;]

(define [n-permute sorted-lst n]
  (define [continue]
    (let* ([f-elt (car sorted-lst)]
           [repeat-cnt (count (curry eq? f-elt) sorted-lst)]
           [remaining (filter-not (curry eq? f-elt) sorted-lst)])
      (append
        (append-map
          (λ [c] (map (λ [result] (append (make-list c f-elt) result))
                      (n-permute remaining (- n c))))
          (range 1 (add1 repeat-cnt)))
        (n-permute remaining n))))

  (let ([len (length sorted-lst)]
        [unique-lst (remove-duplicates sorted-lst)])
    (cond ([= n 0] (list '()))
          ([= n 1] (map list unique-lst))
          ([= n len] (list sorted-lst))
          (else (continue)))))

(define [subsets-with-dup ints]
  (let* ([len (length ints)] [sorted-ints (sort ints <)]
         [result-lens (range 0 (add1 len))])
    (append-map (curry n-permute sorted-ints) result-lens)))

(define test-set '(1 2 2))

(subsets-with-dup test-set)
