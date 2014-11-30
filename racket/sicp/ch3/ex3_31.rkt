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

(define [make-wire]
  (let ([signal-value 0]
        [transformations '()])
    (define [call-each remaining-transformations]
      (unless [null? remaining-transformations]
        ((car remaining-transformations))
        (call-each (cdr remaining-transformations))))
    (define [set-signal! new-signal]
      (unless [eq? signal-value new-signal]
        (set! signal-value new-signal)
        (call-each transformations)))
    
    (define [accept-transformation! new-transformation]
      (set! transformations (cons new-transformation
                                  transformations))
      (new-transformation))
    
    (lambda [op . args]
      (cond ([eq? op 'get-signal] signal-value)
            ([eq? op 'set-signal!] (apply set-signal! args))
            ([eq? op 'add-transformation!] (apply accept-transformation! args))
            (else (error "unknown operation -- WIRE" op))))))
(define [get-signal wire] (wire 'get-signal))
(define [set-signal! wire new-signal] (wire 'set-signal! new-signal))
(define [add-transformation! wire new-transformation]
  (wire 'add-transformation! new-transformation))

(define [make-agenda]
  (let* ([agenda-list (list)]
         [initial-time (current-seconds)]
         [current-time (lambda [] (- (current-seconds) initial-time))]
         [time->agenda-time (lambda [time] (- time initial-time))])
    (lambda [op . args]
      (cond ([eq? op 'empty-agenda?] (null? agenda-list))
            ([eq? op 'first-item] (car agenda-list))
            ([eq? op 'remove-first-item!] (set! agenda-list (cdr agenda-list)))
            ([eq? op 'add-item!]
             (set! agenda-list
               (reverse (cons (cons (time->agenda-time (car args))
                                    (cadr args))
                              (reverse agenda-list)))))
            ([eq? op 'current-time] (current-time))
            ))))
(define [empty-agenda? agenda] (agenda 'empty-agenda?))
(define [first-agenda-item agenda] (agenda 'first-item))
(define [remove-first-item! agenda] (agenda 'remove-first-item!))
(define [add-to-agenda! agenda delay-time action]
  (agenda 'add-item! delay-time action))
(define [current-time agenda] (agenda 'current-time))
(define [delay-trigger agenda delay-interval action]
  (add-to-agenda! agenda
                  (+ (current-seconds) delay-interval)
                  action))
(define [propagate agenda]
  (define [iter]
    (if [empty-agenda? agenda]
      'propagate-done
      (let* ([first-item (first-agenda-item agenda)]
             [first-delay-time (car first-item)]
             [first-action (cdr first-item)])
        (when [<= first-delay-time (current-time agenda)]
          (first-action)
          (remove-first-item! agenda))
        (iter))))
  (iter))

(define global-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)
(define [inverter in-a out-b]
  (define [invert-proc]
    (let ([new-signal (logical-not (get-signal in-a))])
      (delay-trigger global-agenda inverter-delay
                     (lambda [] (set-signal! out-b new-signal)))))
  (add-transformation! in-a invert-proc)
  'inverter-ok)

(define [and-gate in-a in-b out-c]
  (define [and-proc]
    (let ([new-signal (logical-and (get-signal in-a) (get-signal in-b))])
      (delay-trigger global-agenda and-gate-delay
                     (lambda [] (set-signal! out-c new-signal)))))
  (add-transformation! in-a and-proc)
  (add-transformation! in-b and-proc)
  'and-gate-ok)

(define [or-gate in-a in-b out-c]
  (define [or-proc]
    (let ([new-signal (logical-or (get-signal in-a) (get-signal in-b))])
      (delay-trigger global-agenda or-gate-delay
                     (lambda [] (set-signal! out-c new-signal)))))
  (add-transformation! in-a or-proc)
  (add-transformation! in-b or-proc)
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

(provide (all-defined-out))
(define [wire-probe name wire]
  (add-transformation! wire
                       (lambda []
                         (newline)
                         (display name)
                         (display " ")
                         (display (current-time global-agenda))
                         (display "  New-value = ")
                         (display (get-signal wire)))))

(define input-a (make-wire))
(define input-b (make-wire))
(define output-sum (make-wire))
(define output-carry (make-wire))

(wire-probe 'input-a input-a)
(wire-probe 'input-b input-b)
(wire-probe 'sum output-sum)
(wire-probe 'carry output-carry)

(half-adder input-a input-b output-sum output-carry)
(set-signal! input-a 1)
(propagate global-agenda)
