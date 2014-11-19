#lang racket

(define [make-vect x-cor y-cor] (cons x-cor y-cor))
(define [vect-xcor vect] (car vect))
(define [vect-ycor vect] (cdr vector))
(define [vect-add vect-a vect-b]
  (make-vect
    (+ (vect-xcor vect-a) (vect-xcor vect-b))
    (+ (vect-ycor vect-a) (vect-ycor vect-b))))
(define [vect-sub vect-a vect-b]
  (make-vect
    (- (vect-xcor vect-a) (vect-xcor vect-b))
    (- (vect-ycor vect-a) (vect-ycor vect-b))))
(define [vect-scale vect factor]
  (make-vect
    (* factor (vect-xcor vect))
    (* factor (vect-ycor vect))))

(define [make-frame origin edge1 edge2] (list origin edge1 edge2))
(define [frame-origin frame] (car frame))
(define [frame-edge1 frame] (cadr frame))
(define [frame-edge2 frame] (caddr frame))

(define [make-segment start-vect end-vect] (cons start-vect end-vect))
(define [segment-start segment] (car segment))
(define [segment-end segment] (cdr segment))

(define [frame-coord-map frame]
  (lambda [v]
    (vect-add
      (frame-origin frame)
      (vect-add
        (vect-scale
          (vect-xcor v)
          (frame-edge1 frame))
        (vect-scale
          (vect-ycor v)
          (frame-edge2 frame))))))

(define [segment->painter segment-list]
  (lambda [frame]
    (for-each
      (lambda [segment]
        (draw-line
          ((frame-coord-map frame) (start-segment segment))
          ((frame-coord-map frame) (end-segment segment))))
      segment-list)))

