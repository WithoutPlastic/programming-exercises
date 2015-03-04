#lang racket

;Problem:
;Given a collection of candidate numbers (C) and a target number (T), find all
;unique combinations in C where the candidate numbers sums to T.
;
;Each number in C may only be used once in the combination.
;
;Note:
;
;- All numbers (including target) will be positive integers.
;- Elements in a combination (a1, a2, … , ak) must be in non-descending order.
;  (ie, a1 ≤ a2 ≤ … ≤ ak).
;- The solution set must not contain duplicate combinations.
;
;For example, given candidate set 10,1,2,7,6,1,5 and target 8,
;
;A solution set is:
;[1, 7]
;[1, 2, 5]
;[2, 6]
;[1, 1, 6]

(define [combination-sum-ii candidates target]
  (let ([sorted-nums (sort candidates <)])
    (define [with-smallest-selected smallest]
      (define [repeat remainings n]
        (let* ([sum (* smallest n)]
               [slice (make-list n smallest)]
               [append-slice (curry append slice)])
          (define [continue]
            (let ([fst (car remainings)]
                  [rst (cdr remainings)])
              (cond ([< target sum] '())
                    ([= sum target] (list slice))
                    ([= fst smallest]
                     (append (map append-slice
                                  (combination-sum-ii rst (- target sum)))
                             (repeat rst (add1 n))))
                    (else
                      (map append-slice
                           (combination-sum-ii remainings (- target sum)))))))

          (if [null? remainings] '() (continue))))

      (repeat (cdr sorted-nums) 1))

    (define [continue]
      (let* ([smallest (car sorted-nums)]
             [dp-smallest (filter-not (curry = smallest) sorted-nums)])
        (append (with-smallest-selected smallest)
                (combination-sum-ii dp-smallest target))))

    (if [null? sorted-nums] '() (continue))))

(define test-candidates '(10 1 2 7 6 1 5))
(define test-target 8)

(combination-sum-ii test-candidates test-target)
