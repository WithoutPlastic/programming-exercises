#lang racket

;Answer:
;a>
(define [simple-stream-flatten stream]
  (stream-map stream-car
              (stream-filter (lambda [x] [not [stream-null? x]])
                             stream)))

;b> No.
