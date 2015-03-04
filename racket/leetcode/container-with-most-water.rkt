#lang racket

;Problem
;Given n non-negative integers a1, a2, ..., an, where each represents a point at
;coordinate (i, ai). n vertical lines are drawn such that the two endpoints of
;line i is at (i, ai) and (i, 0). Find two lines, which together with x-axis
;forms a container, such that the container contains the most water.
;
;Note: You may not slant the container.


(define [find-container-with-most-water numbers]
  (define [make-select l-idx r-idx]
    (let* ([l-height (list-ref numbers l-idx)]
           [r-height (list-ref numbers r-idx)]
           [water (* (- r-idx l-idx) (min l-height r-height))])
      (cons water (cons l-idx r-idx))))
  (define select-water car)

  (define [merge select-a select-b]
    (define [continue]
      (let ([water-a (select-water select-a)]
            [water-b (select-water select-b)])
        (if [< water-a water-b] select-b select-a)))
    (cond ([null? select-a] select-b)
          ([null? select-b] select-a)
          (else (continue))))

  (let ([len (length numbers)])
    (define [filter-taller-idxs height]
      (define [iter idx]
        (cond ([= idx len] '())
              ([< height (list-ref numbers idx)]
               (cons idx (iter (add1 idx))))
              (else (iter (add1 idx)))))
      (iter 0))
  
    (define [iter l-idx r-idx]
      (let* ([l-height (list-ref numbers l-idx)]
             [r-height (list-ref numbers r-idx)])
        (define [dispatch-valid-idxs low-height low-idx]
          (let* ([taller-idxs (filter-taller-idxs low-height)]
                 [valid-idxs (filter (lambda [x] [and [< x r-idx] [< l-idx x]])
                                     taller-idxs)])
            (if [eq? low-idx 'l]
              (foldl merge '() (map (lambda [x] (iter x r-idx)) valid-idxs))
              (foldl merge '() (map (lambda [x] (iter l-idx x)) valid-idxs)))))

        (merge (make-select l-idx r-idx)
               (cond ([< l-height r-height] (dispatch-valid-idxs l-height 'l))
                     ([< r-height l-height] (dispatch-valid-idxs r-height 'r))
                     (else (merge (dispatch-valid-idxs l-height 'l)
                                  (dispatch-valid-idxs r-height 'r)))))))
    (iter 0 (sub1 len))))

(define test-numbers '(8 3 4 9 1 0 4 7 40 4 140 40 3 9 10 7))

(find-container-with-most-water test-numbers)
