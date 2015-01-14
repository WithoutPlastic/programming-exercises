#lang racket

(require rackunit)
(require rackunit/text-ui)
(require "lazy_evaluator.rkt")

(define file-tests
  (test-suite
    "Unit test of rewrite lazy_evaluator from scratch"

    ;---------------------- VV-PAIRS / PA-PAIRS TESTS -------------------------
    (test-case
      "make vv-pair from variable value"
      (check-equal? (make-vv-pair 'a 0) (mcons 'a 0)))
    (test-case
      "extract variable from vv-pair"
      (check-eq? (vv-pair-variable (make-vv-pair 'a 0)) 'a))
    (test-case
      "extract value from vv-pair"
      (check-eq? (vv-pair-value (make-vv-pair 'a 0)) 0))
    (test-case
      "set variable of vv-pair"
      (let ([test-vv-pair (make-vv-pair 'a 0)])
        (set-vv-pair-variable! test-vv-pair 'b)
        (check-equal? test-vv-pair (mcons 'b 0))))
    (test-case
      "set value of vv-pair"
      (let ([test-vv-pair (make-vv-pair 'a 0)])
        (set-vv-pair-value! test-vv-pair 1)
        (check-equal? test-vv-pair (mcons 'a 1))))

    ;------------------------------ FRAME TESTS -------------------------------
    (test-case
      "make frame from empty list"
      (check-equal? (make-frame '()) the-empty-frame))
    (test-case
      "make frame from list with single element"
      (let ([test-vv-pairs (list (make-vv-pair 'a 0))])
        (check-equal? (make-frame test-vv-pairs)
                      (mcons 'frame (mcons (mcons 'a 0) '())))))
    (test-case
      "make frame from list with elements"
      (let ([test-vv-pairs (list (make-vv-pair 'a 0)
                                 (make-vv-pair 'b 1))])
        (check-equal? (make-frame test-vv-pairs)
                      (mcons 'frame
                             (mcons (mcons 'a 0)
                                    (mcons (mcons 'b 1)
                                           '()))))))

    (let ([gen-empty-frame (lambda [] (mcons 'frame '()))]
          [gen-frame-with-single-element
            (lambda [] (make-frame (list (make-vv-pair 'a 0))))]
          [gen-frame-with-four-elements
            (lambda [] (make-frame (list (make-vv-pair 'a 0)
                                         (make-vv-pair 'b 1)
                                         (make-vv-pair 'c 2)
                                         (make-vv-pair 'd 3))))])
      (test-case
        "copy empty frame"
        (check-equal? (frame-copy the-empty-frame) the-empty-frame))
      (test-case
        "copy single element frame"
        (let ([test-frame (gen-frame-with-single-element)])
          (check-equal? (frame-copy test-frame) test-frame)
          (check-not-eq? (frame-copy test-frame) test-frame)))
      (test-case
        "copy multi elements frame"
        (let ([test-frame (gen-frame-with-four-elements)])
          (check-equal? (frame-copy test-frame) test-frame)
          (check-not-eq? (frame-copy test-frame) test-frame)))
      (test-case
        "search frame via variable, return vv-pair if founded"
        (check-equal? (frame-search (gen-frame-with-four-elements) 'd)
                      (make-vv-pair 'd 3)))
      (test-case
        "search frame via variable, return false if target not founded"
        (check-eq? (frame-search (gen-frame-with-four-elements) 'e)
                   #f))
      (test-case
        "get empty frame all variables"
        (check-eq? (frame-variables the-empty-frame) '()))
      (test-case
        "get empty frame all values"
        (check-eq? (frame-values the-empty-frame) '()))
      (test-case
        "get frame all variable list"
        (check-equal? (frame-variables (gen-frame-with-four-elements))
                      '(a b c d)))
      (test-case
        "get frame all value list"
        (check-equal? (frame-values (gen-frame-with-four-elements))
                      '(0 1 2 3)))
      (test-case
        "get head of a empty frame throw error"
        (check-exn exn:fail? (lambda [] (frame-head the-empty-frame))))
      (test-case
        "get head of non empty frame"
        (check-equal? (frame-head (gen-frame-with-four-elements))
                      (make-vv-pair 'a 0)))
      (test-case
        "get rest of a empty frame throw error"
        (check-exn exn:fail? (lambda [] (frame-rest the-empty-frame))))
      (test-case
        "get rest of single element frame, return empty list"
        (check-equal? (frame-rest (gen-frame-with-single-element))
                      (mcons 'frame '())))
      (test-case
        "get rest of multi elements frame"
        (check-equal? (frame-rest (gen-frame-with-four-elements))
                      (mcons 'frame
                             (mcons (mcons 'b 1)
                                    (mcons (mcons 'c 2)
                                           (mcons (mcons 'd 3) '()))))))
      (test-case
        "add to a empty frame"
        (let ([test-frame (gen-empty-frame)])
          (frame-add! test-frame (make-vv-pair 'a 0))
          (check-equal? test-frame (mcons 'frame (mcons (mcons 'a 0) '())))))
      (test-case
        "add to non empty frame"
        (let ([test-frame (gen-frame-with-four-elements)])
          (frame-add! test-frame (make-vv-pair 'e 4))
          (check-equal? test-frame
                        (mcons 'frame
                        (mcons (mcons 'e 4)
                        (mcons (mcons 'a 0)
                        (mcons (mcons 'b 1)
                        (mcons (mcons 'c 2)
                        (mcons (mcons 'd 3) '())))))))))
      (test-case
        "remove element from single element frame"
        (let ([test-frame (gen-frame-with-single-element)])
          (frame-remove! test-frame 'a)
          (check-equal? test-frame (mcons 'frame '()))))
      (test-case
        "remove element from a empty frame throw a error"
        (check-exn exn:fail? (lambda [] (frame-remove! the-empty-frame 'a))))
      (test-case
        "remove head of multi elements frame"
        (let ([test-frame (gen-frame-with-four-elements)])
          (frame-remove! test-frame 'a)
          (check-equal? test-frame
                        (mcons 'frame
                        (mcons (mcons 'b 1)
                        (mcons (mcons 'c 2)
                        (mcons (mcons 'd 3) '())))))))
      (test-case
        "remove tail of multi elements frame"
        (let ([test-frame (gen-frame-with-four-elements)])
          (frame-remove! test-frame 'c)
          (check-equal? test-frame
                        (mcons 'frame
                        (mcons (mcons 'a 0)
                        (mcons (mcons 'b 1)
                        (mcons (mcons 'd 3) '())))))))
      (test-case
        "remove tail of multi elements frame"
        (let ([test-frame (gen-frame-with-four-elements)])
          (frame-remove! test-frame 'd)
          (check-equal? test-frame
                        (mcons 'frame
                        (mcons (mcons 'a 0)
                        (mcons (mcons 'b 1)
                        (mcons (mcons 'c 2) '())))))))
    ;  (test-case
    ;    "remove a unexisted variable from frame should throw a error"
    ;    (check-exn exn:fail?
    ;               (lambda [] (frame-remove! (gen-frame-with-four-elements)
    ;                                         'g))))
      )
    )
  )
(run-tests file-tests)
