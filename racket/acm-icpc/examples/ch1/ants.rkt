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

(define [find-min-duration stick-length position-list]
  (foldl (λ [pos min-duration] (max min-duration (min pos (- stick-length pos))))
         0 position-list))

(define [find-max-duration stick-length position-list]
  (foldl (λ [pos max-duration ] (max max-duration (max pos (- stick-length pos))))
         0 position-list))


(find-min-duration 10 (list 2 6 7))
(find-max-duration 10 (list 2 6 7))
