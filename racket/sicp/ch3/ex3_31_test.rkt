#lang racket

(require rackunit)
(require rackunit/text-ui)
(require "ex3_31.rkt")

(define file-tests
  (test-suite
    "Tests for SICP exercise 3.31"

    (test-eq? "logical not 0 -> 1" (logical-not 0) 1)
    (test-eq? "logical not 1 -> 0" (logical-not 1) 0 )
  
    (test-eq? "logical and 0 && 0 -> 0" (logical-and 0 0) 0)
    (test-eq? "logical and 0 && 1 -> 0" (logical-and 0 1) 0)
    (test-eq? "logical and 1 && 0 -> 0" (logical-and 1 0) 0)
    (test-eq? "logical and 1 && 1 -> 1" (logical-and 1 1) 1 )
  
    (test-eq? "logical or 0 || 0 -> 0" (logical-or 0 0) 0)
    (test-eq? "logical or 0 || 1 -> 1" (logical-or 0 1) 1)
    (test-eq? "logical or 1 || 0 -> 1" (logical-or 1 0) 1)
    (test-eq? "logical or 1 || 1 -> 1" (logical-or 1 1) 1 )
  
    (test-case
      "wire initial state test"
      (check-eq? (get-signal (make-wire)) 0 "initial signal -> 0"))
    
    (test-case
      "wire set signal test"
      (let ([test-wire (make-wire)])
        (set-signal! test-wire 1)
        (check-eq? (get-signal test-wire) 1 "signal set to 1 -> 1")))
  
    (test-case
      "wire add transformations test"
      (let ([test-wire (make-wire)]
            [test-tranformation (lambda [] 1)])
        (check-eq? (add-transformation! test-wire test-tranformation)
                   (test-tranformation)
                   "added transformation performed immediatelly")))
  
    (test-case
      "wire transformation call each test"
      (let* ([test-wire (make-wire)]
             [trans-called-count 0]
             [new-trans
               (lambda []
                 (set! trans-called-count (add1 trans-called-count)))])
        (add-transformation! test-wire new-trans)
        (add-transformation! test-wire new-trans)
        (add-transformation! test-wire new-trans)
        (set-signal! test-wire 0)
        (check-eq? trans-called-count 3
                   "transformation not called when new signal not changed")
        (set-signal! test-wire 1)
        (check-eq? trans-called-count 6
                   "transformations all called when new signal differed")))
    ;wire-probe not test

    (test-true "agenda initial state test" (empty-agenda? (make-agenda)))
    (test-case
      "add and remove item to agenda test"
      (let ([test-agenda (make-agenda)]
            [test-action (lambda [] 1)]
            [delay-time (current-seconds)])
        (add-to-agenda! test-agenda delay-time test-action)
        (check-false (empty-agenda? test-agenda) "first item added")
        (remove-first-item! test-agenda)
        (check-true (empty-agenda? test-agenda) "first item removed")))

    (test-case
      "agenda item structure test"
      (let ([test-agenda (make-agenda)]
            [test-action (lambda [] 1)]
            [test-time (current-seconds)])
        (add-to-agenda! test-agenda test-time test-action)
        (check-= (car (first-agenda-item test-agenda)) 0
                 1 "first of agenda item should be delay-time")
        (check-eq? (cdr (first-agenda-item test-agenda)) test-action
                   "second of agenda item should be action")))

    (test-case
      "agenda relative time test"
      (let ([test-agenda (make-agenda)]
            [test-action (lambda [] 1)]
            [test-time (current-seconds)]
            [sleep-time 2]
            [test-delay-interval 1])
        (sleep sleep-time)
        (delay-trigger test-agenda test-delay-interval test-action)
        (check-= (car (first-agenda-item test-agenda))
                 (+ sleep-time test-delay-interval)
                 1)))
    (test-eq? "empty agenda propagate test"
              (propagate (make-agenda))
              'propagate-done)

    (test-case
      "agenda propagate until longest delay reached test"
      (let ([test-agenda (make-agenda)]
            [test-action (lambda [] 1)]
            [test-start-time (current-seconds)]
            [test-delay-interval 2]
            [longest-delay-interval 4])
        (delay-trigger test-agenda test-delay-interval test-action)
        (delay-trigger test-agenda longest-delay-interval test-action)
        (propagate test-agenda)
        (check-= (current-seconds)
                 (+ test-start-time longest-delay-interval)
                 1)))

    (test-case
      "agenda propagate do all actions in agenda test"
      (let* ([test-agenda (make-agenda)]
             [action-modified-list '()]
             [test-action-a (lambda [] (set! action-modified-list
                                         (cons 'a action-modified-list)))]
             [test-action-b (lambda [] (set! action-modified-list
                                         (cons 'b action-modified-list)))]
             [test-action-c (lambda [] (set! action-modified-list
                                         (cons 'c action-modified-list)))]
             [test-delay-interval 1])
        (delay-trigger test-agenda test-delay-interval test-action-a)
        (delay-trigger test-agenda test-delay-interval test-action-b)
        (delay-trigger test-agenda test-delay-interval test-action-c)
        (propagate test-agenda)
        (check-equal? action-modified-list
                      '(c b a))))

    (test-case
      "inverter signal test"
      (let ([in-a (make-wire)]
            [out-b (make-wire)])
        (inverter in-a out-b)
        (check-eq? (get-signal out-b) 0
                   "out wire stay initial signal when just defined")

        (propagate global-agenda)
        (check-eq? (get-signal out-b) 1
                   "out wire signal is inverted after delay interval")

        (set-signal! in-a 1)
        (check-eq? (get-signal out-b) 1
                   "input signal change don't reflect output immediatelly")

        (propagate global-agenda)
        (check-eq? (get-signal out-b) 0
                   "out wire signal is inverted again after delay interval")))

    (test-case
      "and gate signal test"
      (let ([in-a (make-wire)]
            [in-b (make-wire)]
            [out-c (make-wire)])
        (and-gate in-a in-b out-c)
        (set-signal! in-a 1)
        (set-signal! in-b 1)
        (propagate global-agenda)
        (check-eq? (get-signal out-c) 1
                   "out wire changed when two input signals turn to 1")))

    (test-case
      "or gate signal test"
      (let ([in-a (make-wire)]
            [in-b (make-wire)]
            [out-c (make-wire)])
        (or-gate in-a in-b out-c)
        (set-signal! in-a 1)
        (propagate global-agenda)
        (check-eq? (get-signal out-c) 1
                   "out wire changed when either input signal turn to 1")
        (set-signal! in-b 1)
        (propagate global-agenda)
        (check-eq? (get-signal out-c) 1
                   "out wire stay 1 when both input signal turn to 1")))
  ))

(run-tests file-tests)

