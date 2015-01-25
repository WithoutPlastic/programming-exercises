#lang racket

(define [distinct? items]
  (cond ([null? items] true)
        ([null? (cdr items)] true)
        ([member (car items) (cdr items)] false)
        (else (distinct? (cdr items)))))

(define [get-father-daughter-shipname-pairs]
  (let ([moore (list 'Moore (amb 'Maryann 'Gabrielle 'Lorna 'Rosalind) 'Lorna)]
        ;[moore (list 'Moore 'Maryann 'Lorna)]
        [hood (list 'Hood 'Melissa 'Gabrielle)]
        [hall (list 'Hall (amb 'Gabrielle 'Maryann 'Lorna) 'Rosalind)]
        [downing (list 'Downing (amb 'Gabrielle 'Maryann 'Lorna 'Rosalind) 'Melissa)]
        [parker (list 'Parker (amb 'Gabrielle 'Lorna 'Rosalind) 'Maryann)])
    (require [distinct? (list (second moore) (second hall)
                              (second downing) (second parker))])
    (require [or [and [eq? (second hall) 'Gabrielle]
                      [eq? (third hall) (second parker)]]
                 [and [eq? (second downing) 'Gabrielle]
                      [eq? (third downing) (second parker)]]])
    (list moore hood hall downing parker)))

(get-father-daughter-shipname-pairs)

;(define [flatmap proc items] (foldr append null (map proc items)))
;
;(define fathers '(Mr.Moore ColonelDowning Mr.Hall Sir.BarnacleHood Dr.Parker))
;(define daughters '(Gabrielle Lorna Rosalind Melissa Maryann))
;(define [permutations available-set]
;  (if [null? available-set]
;    (list null)
;    (flatmap (lambda [x] (map (lambda [y] (cons x y))
;                              (permutations (remove x available-set))))
;             available-set)))
;(define [generate-permutation-matrix]
;  (map (lambda [x] (map list fathers (car x) (cdr x)))
;       (flatmap (lambda [x] (map (lambda [y] (cons x y))
;                                 (permutations daughters)))
;                (permutations daughters))))
;(define [format-solution solution-list] solution-list)
;
;(define [find-gabrielle-father-shipname permutation]
;  (third (car (filter (lambda [x] [eq? (second x) 'Gabrielle]) permutation))))
;(define [valid-solution? permutation]
;  (let ([moore-daughter (second (first permutation))]
;        [moore-shipname (third (first permutation))]
;        [colonel-daughter (second (second permutation))]
;        [colonel-shipname (third (second permutation))]
;        [hall-daughter (second (third permutation))]
;        [hall-shipname (third (third permutation))]
;        [barnacle-daughter (second (fourth permutation))]
;        [barnacle-shipname (third (fourth permutation))]
;        [parker-daughter (second (fifth permutation))]
;        [parker-shipname (third (fifth permutation))])
;    [and [not [eq? moore-daughter moore-shipname]]
;         [not [eq? colonel-daughter colonel-shipname]]
;         [not [eq? hall-daughter hall-shipname]]
;         [not [eq? barnacle-daughter barnacle-shipname]]
;         [not [eq? parker-daughter parker-shipname]]
;         ;[eq? moore-daughter 'Maryann]
;         [eq? barnacle-daughter 'Melissa]
;         [eq? barnacle-shipname 'Gabrielle]
;         [eq? moore-shipname 'Lorna]
;         [eq? hall-shipname 'Rosalind]
;         [eq? colonel-shipname 'Melissa]
;         [eq? parker-shipname 'Maryann]
;         [eq? (find-gabrielle-father-shipname permutation)
;              parker-daughter]]))
;
;(define [generate-solutions]
;  (map format-solution
;       (filter valid-solution? (generate-permutation-matrix))))
;(generate-solutions)
