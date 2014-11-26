#lang racket
(require scheme/mpair)

(define [make-queue] (mcons '() '()))
(define [empty-queue? queue] [null? (front-ptr queue)])
(define [front-ptr queue] (mcar queue))
(define [rear-ptr queue] (mcdr queue))
(define [set-front-ptr! queue item] (set-mcar! queue item))
(define [set-rear-ptr! queue item] (set-mcdr! queue item))

(define [front-queue queue]
  (if [empty-queue? queue]
    (error "access front of empty queue failed." queue)
    (mcar (front-ptr queue))))
(define [insert-queue! queue item]
  (let ([new-pair (mcons item '())])
    (if [empty-queue? queue]
      (begin
        (set-front-ptr! queue new-pair)
        (set-rear-ptr! queue new-pair))
      (begin
        (set-mcdr! (rear-ptr queue) new-pair)
        (set-rear-ptr! queue new-pair)))
    queue))

(define [delete-queue! queue]
  (cond ([empty-queue? queue]
         (error "delete tail element of empty queue failed."))
        ([eq? (front-ptr queue) (rear-ptr queue)] (set! queue (make-queue)))
        (else
          (set-front-ptr! queue (mcdr (front-ptr queue)))))
  queue)

(define [print-queue queue]
  (front-ptr queue))

(define q0 (make-queue))
(print-queue (insert-queue! q0 'a))
(print-queue (insert-queue! q0 'b))
(print-queue (delete-queue! q0))
(print-queue (delete-queue! q0))
