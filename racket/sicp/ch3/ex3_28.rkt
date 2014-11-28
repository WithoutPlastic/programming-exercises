#lang racket

(define [logical-not in]
  (cond ([eq? in 0] 1)
        ([eq? in 1] 0)
        (else (error "invalid logical input -- LOGICAL-NOT" in))))
(define [logical-and in-a in-b]
  (cond ([and (eq? in-a 1) (eq? in-b 1)] 1)
        ([or (eq? in-a 0) (eq? in-b 0)] 0)
        (else (error "invalid logical input -- LOGICAL-AND" in-a in-b))))
(define [logical-or in-a in-b]
  (cond ([or (eq? in-a 1) (eq? in-b 1)] 1)
        ([and (eq? in-a 0) (eq? in-b 0)] 0)
        (else (error "invalid logical input -- LOGICAL-OR" in-a in-b))))

(define [make-wire])
(define [get-signal wire])
(define [set-signal! wire new-signal])
(define [after-delay delay-interval proc-to-be-trigger])
(define [add-transformation wire transformation])

(define [inverter in-a out-b]
  (define [invert-proc]
    (let ([new-signal (logical-not (get-signal in-a))])
      (after-delay inverter-delay
                   (lambda []
                     (set-signal! out-b new-signal)))))
  (add-transformation in-a invert-proc)
  'inverter-ok)

(define [and-gate in-a in-b out-c]
  (define [and-proc]
    (let ([new-signal (logical-and (get-signal in-a)
                                   (get-signal in-b))])
      (after-delay and-gate-delay
                   (lambda []
                     (set-signal! out-c new-signal)))))
  (add-transformation in-a and-proc)
  (add-transformation in-b and-proc)
  'and-gate-ok)

(define [or-gate in-a in-b out-c]
  (define [or-proc]
    (let ([new-signal (logical-or (get-signal in-a)
                                  (get-signal in-b))])
      (after-delay or-gate-delay
                   (lambda []
                     (set-signal! out-c new-signal)))))
  (add-transformation in-a or-proc)
  (add-transformation in-b or-proc)
  'or-gate-ok)

(define [half-adder in-a in-b out-sum out-cout]
  (let ([inner-c (make-wire)]
        [inner-d (make-wire)])
    (or-gate in-a in-b inner-c)
    (and-gate in-a in-b out-cout)
    (inverter out-cout inner-d)
    (and-gate inner-c inner-d out-sum)
    'half-adder-ok))

(define [full-adder in-a in-b in-cin out-sum out-cout]
  (let ([inner-c (make-wire)]
        [inner-d (make-wire)]
        [inner-e (make-wire)]
        )
    (half-adder in-b in-cin inner-c inner-d)
    (half-adder in-a inner-c out-sum inner-e)
    (or-gate inner-d inner-e out-cout)
    'full-adder-ok))
