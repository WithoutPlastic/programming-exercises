#lang racket

(define [a-pythagorean-triple-from low-boundary]
  (let* ([i (an-integer-starting-from low-boundary)]
         [i-square (* i i)]
         [j (an-integer-between i (/ (- i-square 1) 2))]
         [j-square (* j j)]
         [k (sqrt ( + i-square j-square))])
    (require (integer? k))
    (list i j k)))

;Answer: If we simply replace an-integer-between with an-integer-starting-from,
;it will eval a infinite integer stream which will block before generate all
;pythagorean triple successfully. But once i is determined, the largest k value
;is also determined. Obviously, k should be an integer larger than j, once k
;equal j + 1, and then minus between k-square and j-square larger than i-square,
;then all those larger j/k is needless to try.
;So assume k equals to j + 1, i^2 + j^2 = (j + 1)^2, we can get j upper boundary
;be (i-square - 1) / 2, finally, got possible integer k met pythagorean
;requirement.
