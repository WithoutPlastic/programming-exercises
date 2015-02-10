#lang racket

;Problem:
;Given an array S of n integers, are there elements a, b, c in S such that a + b
;+ c = 0? Find all unique triplets in the array which gives the sum of zero.
;
;Note:
;Elements in a triplet (a,b,c) must be in non-descending order. (ie, a ≤ b ≤ c)
;The solution set must not contain duplicate triplets.
;
;For example, given array S = {-1 0 1 2 -1 -4},
;
;A solution set is:
;(-1, 0, 1)
;(-1, -1, 2)))))

(define [remove-one-elts-from elts target eq-pred?]
  (define [iter remainings]
    (cond ([null? remainings] '())
          ([eq-pred? (car remainings) target] (cdr remainings))
          (else (cons (car remainings) (iter (cdr remainings))))))
  (iter elts))
(define [remove-two-elts-from ints target-a target-b eq-pred?]
  (remove-one-elts-from
    (remove-one-elts-from ints target-a eq-pred?)
    target-b
    eq-pred?))

(define [find-three-sum-zero ints]
  (let* ([below-zeros (filter (lambda [x] [< x 0]) ints)]
         [above-zeros (filter (lambda [x] [< 0 x]) ints)]
         [unq-b-zeros (remove-duplicates below-zeros)]
         [unq-a-zeros (remove-duplicates above-zeros)])
    (define [iter remaining-belows remaining-aboves]
      (define [continue]
        (let ([first-below (car remaining-belows)]
              [first-above (car remaining-aboves)]
              [rest-belows (cdr remaining-belows)]
              [rest-aboves (cdr remaining-aboves)])
          (define [find-with target]
            (define [iter remainings]
              (cond ([null? remainings] '())
                    ([< target 0]
                     (append (find-with-ba target (car remainings))
                             (iter (cdr remainings))))
                    (else
                     (append (find-with-ba (car remainings) target)
                             (iter (cdr remainings))))))

            (if [< target 0] (iter rest-aboves) (iter rest-belows)))

          (define [find-with-ba below above]
            (let* ([w/o-ba-ints (remove-two-elts-from ints below above =)]
                   [between-ba? (lambda [x] [and [<= x above] [<= below x]])]
                   [between-ba-ints (filter between-ba? w/o-ba-ints)]
                   [sum (lambda [x] (+ below above x))]
                   [sum-equal-zero? (lambda [x] [= (sum x) 0])]
                   [matchs (filter sum-equal-zero? between-ba-ints)])
              (if [null? matchs] '() (list (list below (car matchs) above)))))

          (append (find-with first-below) (find-with first-above)
                  (find-with-ba first-below first-above)
                  (iter rest-belows rest-aboves))))

      (if [and [pair? remaining-belows] [pair? remaining-aboves]]
        (continue) '()))

    (let ([result (iter unq-b-zeros unq-a-zeros)])
      (if [< 2 (length (filter (lambda [x] [= x 0]) ints))]
        (cons '(0 0 0) result)
        result))))

(define test-ints-a '(-1 0 1 2 -1 -4))
(define test-ints-b '(-1 0 0 0 -1 1 2 -1 -4 7 12 -8 -3 -3 -10))

(find-three-sum-zero test-ints-b)
