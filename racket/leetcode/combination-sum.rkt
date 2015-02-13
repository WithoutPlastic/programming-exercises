#lang racket

;Problem:
;Given a set of candidate numbers (C) and a target number (T), find all unique
;combinations in C where the candidate numbers sums to T.
;
;The same repeated number may be chosen from C unlimited number of times.
;
;Note:
;- All numbers (including target) will be positive integers.
;- Elements in a combination (a1, a2, … , ak) must be in non-descending order.
;  (ie, a1 ≤ a2 ≤ … ≤ ak).
;- The solution set must not contain duplicate combinations.
;
;For example, given candidate set 2,3,6,7 and target 7,
;
;A solution set is:
;
;[7]
;[2, 2, 3]

(define [combination-sum candidates target]
  (let* ([sorted-nums (sort (remove-duplicates candidates) <)]
         [valids (filter (curryr <= target) sorted-nums)])
    (define [with-smallest-selected smallest]
      (define [repeat n]
        (let ([slice (make-list n smallest)]
              [repeat-sum (* smallest n)])
          (define [continue]
            (append (map (curry append slice)
                         (combination-sum (cdr valids) (- target repeat-sum)))
                    (repeat (add1 n))))

          (cond ([= repeat-sum target] (list slice))
                ([< repeat-sum target] (continue))
                (else '()))))

      (repeat 1))

    (define [continue]
      (let ([s-num (car valids)])
        (append (with-smallest-selected s-num)
                (combination-sum (cdr valids) target))))

    (if [null? valids] '() (continue))))

(define test-candidates '(2 3 6 7))
(define test-target-a 7)
(define test-target-b 13)

(combination-sum test-candidates test-target-a)
(combination-sum test-candidates test-target-b)
