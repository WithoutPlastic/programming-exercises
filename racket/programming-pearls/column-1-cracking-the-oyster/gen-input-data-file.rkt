#lang racket

(require "./int-bytes-convert.rkt")

(call-with-output-file
  "input.data"
  #:exists 'truncate
  (λ [out] (for-each (curryr write-bytes out) (map int->4bytes (shuffle (range 0 1000000))))))
