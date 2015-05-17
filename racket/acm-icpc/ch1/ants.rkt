#lang racket

;Problem:
;Assuming that there is n ants moving at 1 cm/s speed on stick with L length(cm). Each position is
;xi, direction unknown. Ant with drop on both end. Since the stick is thin, neighbor ant pair will
;not go across each other. Both of then will turn to oppsite direction.
;
;Please find the min and max duration that all ants dropped.
;
;Restriction:
;1 <= L <= 1000000
;1 <= n <= 1000000
;0 <= xi <= L

(define [permute-direction n]
  (let iter ([remaining n] [direction-permutation (list '())])
    (if [<= remaining 0] direction-permutation
      (iter (sub1 remaining)
            (append-map (λ [d] (list (cons true d) (cons false d)))
                        direction-permutation)))))

(define [get-duration stick-length position-list direction-list]
  (let* ([dir-pos-pair-list (map cons direction-list position-list)]
         [leftmost-pair-with-right-direction (findf (λ [p] (car p)) dir-pos-pair-list)]
         [rightmost-pair-with-left-direction (findf (λ [p] [not (car p)])
                                                    (reverse dir-pos-pair-list))])
    (max (if [not leftmost-pair-with-right-direction] 0
           (- stick-length (cdr leftmost-pair-with-right-direction)))
         (if [not rightmost-pair-with-left-direction] 0
           (cdr rightmost-pair-with-left-direction)))))

(define [find-all-duration-permutation stick-length position-list]
  (let* ([n (length position-list)]
         [direction-permutation (permute-direction n)])
    (map (curry get-duration stick-length position-list) direction-permutation)))

(define [find-min-duration stick-length position-list]
  (apply min (find-all-duration-permutation stick-length position-list)))

(define [find-max-duration stick-length position-list]
  (apply max (find-all-duration-permutation stick-length position-list)))


(find-min-duration 10 (list 2 6 7))
(find-max-duration 10 (list 2 6 7))
