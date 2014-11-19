#lang racket

(define wave2 (beside wave (flip-vert wave)))

(define wave4 (below wave2 wave2))

(define [flipped-pairs painter]
  (let ([painter2 (beside painter (flip-vert painter))])
    (below painter2 painter2)))

(define wave4 (flipped-pairs wave))

(define [right-split painter n]
  (if [= n 0]
    painter
    (let ([smaller (right-split painter (- n 1))])
      (beside painter (below smaller smaller)))))

(define [up-split painter n]
  (if [= n 0]
    painter
    (let ([smaller (up-split painter (- n 1))])
      (below painter (beside smaller smaller)))))

(define [split increase-op split-op]
  (lambda [painter n]
    (if [= n 0]
      painter
      (let ([smaller ((split increase-op split-op) painter (- n 1))])
        (increase-op painter (split-op smaller smaller))))))

(define [corner-split painter n]
  (if [= n 0]
    painter
    (let* ([up (up-split painter (- n 1))]
          [right (right-split painter (- n 1))]
          [top-left (beside up up)]
          [bottom-right (below right right)]
          [corner (corner-split painter (- n 1))])
      (beside (below painter top-left)
              (below bottom-right corner)))))

(define [square-limit painter n]
  (let* ([quarter (corner-split painter n)]
         [half (beside (flip-horiz quarter) quarter)])
    (below (flip-vert half) half)))

(define [square-of-four tl tr bl br]
  (lambda [painter]
    (let ([top (beside (tl painter) (tr painter))]
          [bottom (beside (bl painter) (br painter))])
      (below bottom top))))

(define [flipped-pairs painter]
  (square-of-four identity flip-vert identity flip-vert))

(define [square-limit painter n]
  ((square-of-four flip-horiz identity rotate180 flip-vert)
   (corner-split painter n)))
