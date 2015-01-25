#lang racket

(define [multiple-dewelling]
  (let ([cooper (amb 2 3 4 5)]
        [miller (amb 3 4 5)])
    (require (< cooper miller))
    (let ([fletcher (amb 2 3 4)])
      (require [not [= (abs (- fletcher cooper)) 1]])
      (let ([smith (amb 1 2 3 4 5)])
        (require [not [= (abs (- fletcher smith)) 1]])
        (let ([baker (amb 1 2 3 4)])
          (require [distinct? (list baker cooper fletcher miller smith)])
          (list (list 'baker baker)
                (list 'cooper cooper)
                (list 'fletcher fletcher)
                (list 'miller miller)
                (list 'smith smith)))))))

;Answer: Before I do exercise, I have ever go thraight to constructing
;non-determinal evaluator, I gonna to consider the possibility of overlapping
;amb and require expressions which should be more efficiency. And this exercise
;is give reader a metaphor about this. We should put both low-overhead and
;more-restrictive requirement combine with amb expression ahead of others. Also
;some direct code simplify also can be taken.
;
;The combination of let-expression with amb keyword might be simplified in
;evaluator?
