#lang racket

;Problem:
;Given two strings S and T, determine if they are both one edit distance apart.
;
;Hint:
;
; - If | n – m | is greater than 1, we know immediately both are not one-edit
;   distance apart.
; - It might help if you consider these cases separately, m == n and m ≠ n.
; - Assume that m is always ≤ n, which greatly simplifies the conditional
;   statements. If m > n, we could just simply swap S and T.
; - If m == n, it becomes finding if there is exactly one modified operation.
;   If m ≠ n, you do not have to consider the delete operation. Just consider
;   the insert operation in T.

(define [drop-elt-idx lst idx] (append (take lst idx) (drop lst (add1 idx))))

(define [one-edit-distance? str-s str-t]
  (let ([s-len (string-length str-s)]
        [t-len (string-length str-t)]
        [chars-s (string->list str-s)]
        [chars-t (string->list str-t)])
    (cond ([< 1 (abs (- s-len t-len))] false)
          ([< s-len t-len]
           [ormap (curry equal? chars-s)
                  (map (curry drop-elt-idx chars-t) (range 0 t-len))])
          ([= s-len t-len]
           [<= (length (filter-map (negate eq?) chars-s chars-t)) 1])
          (else (one-edit-distance? str-t str-s)))))

(one-edit-distance? "hello world" "hello world")
(one-edit-distance? "hello world" "hello world!")
(one-edit-distance? "hello racket" "hello world!")
(one-edit-distance? "hello racket" "hello")
