#lang racket

;Problem:
;Given an array of words and a length L, format the text such that each line
;has exactly L characters and is fully (left and right) justified.
;
;You should pack your words in a greedy approach; that is, pack as many words
;as you can in each line. Pad extra spaces ' ' when necessary so that each line
;has exactly L characters.
;
;Extra spaces between words should be distributed as evenly as possible. If the
;number of spaces on a line do not divide evenly between words, the empty slots
;on the left will be assigned more spaces than the slots on the right.
;
;For the last line of text, it should be left justified and no extra space is
;inserted between words.
;
;For example,
;words: ["This", "is", "an", "example", "of", "text", "justification."]
;L: 16.
;
;Return the formatted lines as:
;
;[
;"This    is    an",
;"example  of text",
;"justification.  "
;]
;
;Note: Each word is guaranteed not to exceed L in length.

(define space #\space)
(define n-space (curryr make-string space))

(define [split-by-l words l]
  (define [iter remaining accum total-accum]
    (define [continue]
      (let* ([f-w (car remaining)] [r-ws (cdr remaining)]
             [first-word-len (string-length f-w)]
             [len-sum (apply + (map (compose add1 string-length) accum))])
        (if [< l (+ len-sum first-word-len)]
          (iter r-ws (list f-w) (append total-accum (list accum)))
          (iter r-ws (append accum (list f-w)) total-accum))))

    (if [null? remaining] (append total-accum (list accum)) (continue)))

  (iter words '() '()))

(define [full-justify words l]
  (let* ([splited-words (split-by-l words l)]
         [w/o-last-line (drop-right splited-words 1)]
         [last-line (last splited-words)])
    (define [fill-spaces words]
      (let* ([total-sp (- l (apply + (map string-length words)))]
             [slt-cnt (sub1 (length words))]
             [r (remainder total-sp slt-cnt)])
        (define [iter remaining cnt]
          (let* ([sp-num (+ (floor (/ total-sp slt-cnt)) (if [< cnt r] 1 0))]
                 [insert-sp-slice (n-space sp-num)])
            (if [< cnt slt-cnt]
              (cons (car remaining)
                    (cons insert-sp-slice (iter (cdr remaining) (add1 cnt))))
              (list (car remaining)))))

        (iter words 0)))

    (map string-append* (append (map fill-spaces w/o-last-line)
                                (list (add-between last-line (n-space 1)))))))

(define test-words '("This" "is" "an" "example" "of" "text" "justification."))
(define test-l 16)

(full-justify test-words 16)
