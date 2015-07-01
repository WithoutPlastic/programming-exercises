#lang racket

;Problem(POJ 3617):
;Given string S with length N, construct string T with the same length. At the beginning, T is
;empty string, and perform belowing action:
;
;  - Delete char at head of S, join to tail of T.
;  - Delete char at tail of S, join to tail of T.
;
;Now, we should make sure the result T owning smallest dict index.
;
;Restriction:
;0 <= N <= 2000
;String only contain uppercase char.


(define [construct-smallest-dict-string str]
  (let ([char-lst (string->list str)])
    (let iter ([char-lst char-lst] [result-char-lst '()])
      (cond ([null? char-lst] (list->string result-char-lst))
            ([< (char->integer (car char-lst)) (char->integer (last char-lst))]
             (iter (cdr char-lst) (append result-char-lst (list (car char-lst)))))
            (else (iter (drop-right char-lst 1)
                        (append result-char-lst (list (last char-lst)))))))))


(construct-smallest-dict-string "ACDBCB")
