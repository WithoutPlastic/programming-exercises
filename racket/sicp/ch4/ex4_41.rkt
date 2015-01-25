#lang racket

(define [flatmap proc items] (foldr append null (map proc items)))

(define [permutations available-set]
  (if [null? available-set]
    (list null)
    (flatmap (lambda [x] (map (lambda [y] (cons x y))
                              (permutations (remove x available-set))))
             available-set)))

(define [format-solution solution-list]
  (map list '(baker cooper fletcher miller smith) solution-list))

(define [valid-solution? permutation]
  (let ([b (first permutation)]
        [c (second permutation)]
        [f (third permutation)]
        [m (fourth permutation)]
        [s (fifth permutation)])
    [and [not [= b 5]] [not [= c 1]] [not [= f 1]] [not [= f 5]] [< c m]
         [not [= (abs (- s f)) 1]]
         [not [= (abs (- f c)) 1]]]))

(define [multi-dwelling]
  (map format-solution
       (filter valid-solution? (permutations (list 1 2 3 4 5)))))
  
(multi-dwelling)
