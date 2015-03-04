#lang racket

;Problem:
;Given n non-negative integers representing an elevation map where the width of
;each bar is 1, compute how much water it is able to trap after raining.
;
;For example,
;Given [0,1,0,2,1,0,1,3,2,1,2,1], return 6.
;
;The above elevation map is represented by array [0,1,0,2,1,0,1,3,2,1,2,1]. In
;this case, 6 units of rain water (blue section) are being trapped. Thanks
;Marcos for contributing this image!)]]

(define [first-elt-idx lst pred]
  (let ([len (length lst)])
    (define [iter idx]
      (cond ([<= len idx] -1)
            ([pred (list-ref lst idx)] idx)
            (else (iter (add1 idx)))))

    (iter 0)))

(define [last-elt-idx lst pred]
  (let ([len (length lst)])
    (define [iter idx]
      (cond ([< idx 0] -1)
            ([pred (list-ref lst idx)] idx)
            (else (iter (sub1 idx)))))

    (iter (sub1 len))))

(define [trap ints]
  (let ([max-int (apply max ints)])
    (apply
      +
      (map
        (lambda [x]
          (let ([l-idx (first-elt-idx ints (curry <= x))]
                [r-idx (last-elt-idx ints (curry <= x))])
            (if [< l-idx r-idx]
              (length
                (filter (curryr < x)
                        (take (drop ints (add1 l-idx)) (sub1 (- r-idx l-idx)))))
              0)))
        (range 1 (add1 max-int))))))

(define test-ints '(0 1 0 2 1 0 1 3 2 1 2 1))

(trap test-ints)
