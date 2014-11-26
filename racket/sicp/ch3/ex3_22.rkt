#lang racket

(define [make-queue]
  (define queue (mcons '() '()))
  (define [front-ptr] (mcar queue))
  (define [rear-ptr] (mcdr queue))
  (define [set-front-ptr! item] (set-mcar! queue item))
  (define [set-rear-ptr! item] (set-mcdr! queue item))
  (define [empty-queue?] [null? (front-ptr)])
  (define [insert item]
    (let ([new-pair (mcons item '())])
      (if [empty-queue?]
        (begin
          (set-front-ptr! new-pair)
          (set-rear-ptr! new-pair))
        (begin
          (set-mcdr! (rear-ptr) new-pair)
          (set-rear-ptr! new-pair)))))
  (define [delete]
    (cond ([empty-queue?] (error "delete tail element of empty queue failed."))
          ([eq? (front-ptr) (rear-ptr)]
           (set! queue (mcons '() '())))
          (else (set-front-ptr! (mcdr (front-ptr))))))
  (define print front-ptr)

  (lambda [op . args]
    (cond ([eq? op 'empty?] (empty-queue?))
          ([eq? op 'insert] (apply insert args) queue)
          ([eq? op 'delete] (delete) queue)
          ([eq? op 'print] (print))
          ([eq? op 'front] (mcar (front-ptr)))
          (else (error "undefined operation" op)))))

(define q (make-queue))
(q 'insert 'a)
(q 'print)
(q 'insert 'b)
(q 'print)
(q 'insert 'c)
(q 'print)
(q 'delete)
(q 'print)
(q 'delete)
(q 'print)
(q 'delete)
(q 'print)
