#lang racket

(call-with-output-file
  "input.data"
  #:exists 'truncate
  (λ [out] (for-each (curryr displayln out) (shuffle (range 0 100000)))))
