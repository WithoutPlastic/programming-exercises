#lang racket

(define [unique-assertion pattern frame-stream]
  (stream-flatmap
    (lambda [frame]
      (let ([s (qeval (negated-query pattern) (singleton-stream frame))])
        (if [singleton-stream? s] s the-empty-stream)))
    frame-stream))

(put 'unique 'qeval unique-assertion)

(define [singleton-stream? x]
  [and [not [stream-null? x]] [stream-null? (stream-cdr x)]])
