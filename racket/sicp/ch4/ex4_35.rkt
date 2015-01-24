#lang racket

(define [an-integer-between low-boundary high-boundary]
  (require (<= low-boundary high-boundary))
  (amb low-boundary (an-integer-between (+ low-boundary 1) high-boundary)))

;Since racket doesn't support amb keyword, not able to test demo code in text
;book
