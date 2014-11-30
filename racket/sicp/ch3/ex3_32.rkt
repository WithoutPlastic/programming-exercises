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
(define [print-queue queue] (front-ptr queue))


(define [make-time-segment time queue] (cons time queue))
(define [segment-time segment] (car segment))
(define [segment-queue segment] (cdr segment))


(define [make-agenda] (mcons 0 '()))
(define [current-time agenda] (mcar agenda))
(define [set-current-time! agenda time] (set-mcar! agenda time))
(define [segments agenda] (mcdr agenda))
(define [set-segments! agenda segments] (set-mcdr! agenda segments))
(define [first-segment agenda] (mcar (segments agenda)))
(define [rest-segments agenda] (mcdr (segments agenda)))
(define [empty-agenda? agenda] [null? (segments agenda)])
(define [add-to-agenda! agenda time action]
  (define [belongs-before? segments]
    [or [null? segments]
        [< time (segment-time (mcar segments))]])
  (define [make-new-time-segment time action]
    (let ([queue (make-queue)])
      (insert-queue! queue action)
      (make-time-segment time queue)))
  (define [add-to-segments! segments]
    (if [= (segment-time (car segments)) time]
      (insert-queue! (segment-queue (car segments))
                     action)
      (let ([rest (cdr segments)])
        (if [belongs-before? rest]
          (set-mcdr! segments (cons (make-new-time-segment time action)
                                    segments))
          (add-to-segments! segments)))))

  (let ([segments (segments agenda)])
    (if [belongs-before? segments]
      (set-segments! agenda
                     (cons (make-new-time-segment time action)
                           segments))
      (add-to-segments! segments))))

(define [remove-first-agenda-item agenda]
  (let ([queue (segment-queue (first-segment agenda))])
    (delete-queue! queue)
    (when [empty-queue? queue]
      (set-segments! agenda (rest-segments agenda)))))

(define [first-agenda-item agenda]
  (if [empty-agenda? agenda]
    (error "Agenda is empty -- FIRST-AGENDA-ITEM")
    (let ([first-seg (first-segment agenda)])
      (set-current-time! agenda (segment-time first-seg))
      (front-queue (segment-queue first-seg)))))
