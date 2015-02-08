#lang racket

;Problem:
;The string "PAYPALISHIRING" is written in a zigzag pattern on a given number of
;rows like this: (you may want to display this pattern in a fixed font for
;better legibility)
;
;  P   A   H   N
;  A P L S I I G
;  Y   I   R
;
;And then read line by line: "PAHNAPLSIIGYIR"
;Write the code that will take a string and make this conversion given a number
;of rows:
;
;> string convert(string text, int nRows);
;
;convert("PAYPALISHIRING", 3) should return "PAHNAPLSIIGYIR".

(define [zigzag-encode str row]
  (let* ([char-list (string->list str)]
         [len (length char-list)])
    (define [iter idx]
      (define [extract-elts]
        (define [construct cnt step-formula]
          (let ([cur-idx (step-formula idx cnt)])
            (if [< cur-idx len]
              (cons (list-ref char-list cur-idx)
                    (construct (add1 cnt) step-formula))
              '())))
        (construct 0
                   (if [or [= idx 0] [= idx (sub1 row)]]
                     (lambda [init-idx cnt]
                       (+ init-idx (* cnt (- (* 2 row) 2))))
                     (lambda [init-idx cnt]
                       (+ init-idx
                          (* (floor (/ cnt 2)) (- (* 2 row) 2))
                          (* (remainder cnt 2) (* 2 (sub1 (- row idx)))))))))
      (if [< idx row]
        (append (extract-elts) (iter (add1 idx)))
        '()))

    (if [= row 1] str (list->string (iter 0)))))

(define test-string "PAYPALISHIRING")

(zigzag-encode test-string 3)
