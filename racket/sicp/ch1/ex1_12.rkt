#lang racket

(define (pascal row seq)
  (cond ((< row 3) 1)
        ((= seq row) 1)
        ((= seq 1) 1)
        ((< seq row) 
          (+
            (pascal (- row 1) seq)
            (pascal (- row 1) (- seq 1))))))

(pascal 1 1)
(pascal 2 1)
(pascal 2 2)
(pascal 5 3)
(pascal 6 3)
(pascal 6 4)
(pascal 7 4)
