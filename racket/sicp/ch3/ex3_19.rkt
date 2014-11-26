#lang racket

(define [tortoise-hare-cycle? lst]
  (let ([tortoise (cdr lst)]
        [hare (cddr lst)]
        [mu 0]
        [lam 0])
    (define [find-eq-iter]
      (cond ([null? (cdr hare)] #f)
            ([not (eq? tortoise hare)]
             (begin
               (set! tortoise (cdr tortoise))
               (set! hare (cddr hare))
               (find-eq-iter)))
            (else #t)))
    (define [find-cycle-start-iter]
      (set! tortoise lst)
      (when [not (eq? tortoise hare)]
        (begin
          (set! mu (add1 mu))
          (set! tortoise (cdr tortoise))
          (set! hare (cdr hare))
          (find-cycle-start-iter))))
    (define [find-lam-iter]
      (set! hare (cdr tortoise))
      (set! lam 1)
      (when [not (eq? tortoise hare)]
        (begin
          (set! lam (add1 lam))
          (set! hare (cdr hare))
          (find-lam-iter))))

    (if [not find-eq-iter]
      #f
      (begin
        (find-cycle-start-iter)
        (find-lam-iter)
        (list mu lam)))))

(define [two-power-cycle? lst]
  (let* ([cur-power 1]
         [lam 1]
         [mu 0]
         [hare (cdr lst)]
         [tortoise hare])
    (define [repeat-f f x times]
      (if [= 0 times]
        (repeat-f f (f x) (sub1 times))
        x))
    (define [find-lam-iter]
      (cond ([null? (cdr hare)] #f)
            ([and (not (= tortoise hare)) (= cur-power lam)]
             (set!-values
               (tortoise cur-power lam)
               (hare (* cur-power 2) 0)))
            ([not (eq? tortoise hare)]
             (begin
               (set!-values
                 (hare lam)
                 ((cdr hare) (add1 lam)))
               (find-lam-iter)))
            (else
              (begin
                (set!-values (tortoise hare) (lst lst))
                #t))))
    (define [find-mu-iter]
      (if [eq? tortoise hare]
        mu
        (set!-values (tortoise hare mu)
                     ((cdr tortoise) (cdr hare) (add1 mu)))))
    (if [not (find-lam-iter)]
      #f
      (begin
        (set! hare (repeat-f cdr hare lam))
        (find-mu-iter)
        (list mu lam)))))
