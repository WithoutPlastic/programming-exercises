#lang racket

(define [distinct? items]
  (cond ([null? items] true)
        ([null? (cdr items)] true)
        ([member (car items) (cdr items)] false)
        (else (distinct? (cdr items)))))

(define [multi-dwelling]
  (let* ([baker '(1 2 3 4 5)]
         [cooper '(1 2 3 4 5)]
         [fletcher '(1 2 3 4 5)]
         [miller '(1 2 3 4 5)]
         [smith '(1 2 3 4 5)]
         [bcs (append* (map (lambda [b] (map (lambda [c] (list b c)) cooper))
                           baker))]
         [bcfs (append* (map (lambda [bc] (map (lambda [f] (list bc f)) fletcher))
                            bcs))]
         [bcfms (append* (map (lambda [bcf] (map (lambda [m] (list bcf m)) miller))
                              bcfs))]
         [bcfmss (append* (map (lambda [bcfm] (map (lambda [s] (list bcfm s)) smith))
                               bcfms))]
         [combinations (map (lambda [x] (list (caaaar x)
                                              (cadr (caaar x))
                                              (cadaar x)
                                              (cadar x)
                                              (cadr x))) bcfmss)])
    ;(display combinations)
    (define [iter remaining]
      (if [null? remaining]
        (display "DONE")
        (let* ([combination (first remaining)]
               [rests (rest remaining)]
               [b (first combination)]
               [c (second combination)]
               [f (third combination)]
               [m (fourth combination)]
               [s (fifth combination)])
          (when [and [not [= b 5]]
                   [not [= c 1]]
                   [not [= f 1]]
                   [not [= f 5]]
                   [< c m]
                   [not [= (abs (- s f)) 1]]
                   [not [= (abs (- f c)) 1]]
                   [distinct? combination]]
            (begin (newline)(newline)(display combination)))
          (iter rests))))
    (iter combinations)))
(multi-dwelling)
