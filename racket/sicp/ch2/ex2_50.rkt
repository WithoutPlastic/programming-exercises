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

(define [segments->painter segment-list]
  (lambda [frame]
    (for-each
      (lambda [segment]
        (draw-line
          ((frame-coord-map frame) (start-segment segment))
          ((frame-coord-map frame) (end-segment segment))))
      segment-list)))

(define [border-painter frame]
  (let* ([point-a (frame-origin frame)]
         [point-b (vect-add (frame-edge1 frame) point-a)]
         [point-c (vect-add (frame-edge2 frame) point-a)]
         [point-d (vect-add point-a (frame-edge1 frame) (frame-edge2 frame))])
    ((segments-painter (list (make-segment point-a point-b)
                             (make-segment point-a point-c)
                             (make-segment point-b point-d)
                             (make-segment point-c point-d)))
     frame)))

(define [cross-painter frame]
  (let* ([point-a (frame-origin frame)]
         [point-b (vect-add (frame-edge1 frame) point-a)]
         [point-c (vect-add (frame-edge2 frame) point-a)]
         [point-d (vect-add point-a (frame-edge1 frame) (frame-edge2 frame))])
    ((segments-painter (list (make-segment point-a point-d)
                             (make-segment point-c point-b)))
     frame)))

(define [diamond-painter frame]
  (let* ([point-a (vect-add
                    (frame-origin frame)
                    (vect-scale (frame-edge1 frame) 0.5))]
         [point-b (vect-add
                    (frame-origin frame)
                    (vect-scale (frame-edge2 frame)0.5))]
         [point-c (vect-add
                    (frame-origin frame)
                    (vect-add
                      (frame-edge1 frame)
                      (vect-scale (frame-edge2 frame) 0.5)))]
         [point-d (vect-add
                    (frame-origin frame)
                    (vect-add
                      (frame-edge2 frame)
                      (vect-scale (frame-edge1 frame) 0.5)))])
    ((segments-painter (list (make-segment point-a point-b)
                             (make-segment point-a point-c)
                             (make-segment point-b point-c)
                             (make-segment point-c point-d)))
     frame)))

(define [transform-painter painter origin corner1 corner2]
  (lambda [frame]
    (let* ([m (frame-coord-map frame)]
           [new-origin (m origin)])
      (make-frame new-origin
                  (vect-sub (m corner1) new-origin)
                  (vect-sub (m corner2) new-origin)))))

(define [flip-vert painter]
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(define [flip-horiz painter]
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(define [rotate-180 painter]
  (transform-painter painter
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 0.0)))

(define [rotate-270 painter]
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(define [beside painter-left painter-right]
  (let* ([split-point (make-vect 0.5 0.0)])
    (lambda [frame]
      ((transform-painter painter-left
                          (make-vect 0.0 0.0)
                          split-point
                          (make-vect 0.0 1.0))
       frame)
      ((transform-painter painter-right
                          split-point
                          (make-vect 1.0 0.0)
                          (make-vect 0.5 1.0))
       frame))))
