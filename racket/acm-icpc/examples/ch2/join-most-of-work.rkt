#lang racket

;Problem:
;There is N work scheduled in plan. Each one start from Si, end to Ti. For every work, you can
;choose to join in or not. Once joined in, you must doing it until it ended. So that the scheduled
;work can't overlap(end time equal start time treat as overlap case).
;
;Try to find out the case, you join as many work as you can.
;
;Restriction:
;0 <= N <= 10^6
;0 <= Si, Ti <= 10^9


(define [find-out-max-non-overlap-work schedule-lst]
  (let ([end-time-ascending-schedule-lst (sort schedule-lst (λ [a b] [< (cdr a) (cdr b)]))])
    (let iter ([counter 0] [schedule-lst end-time-ascending-schedule-lst])
      (if [null? schedule-lst] counter
        (iter (add1 counter)
              (filter (λ [schedule] [< (cdar schedule-lst) (car schedule)]) schedule-lst))))))


(find-out-max-non-overlap-work (list (cons 1 3) (cons 2 5) (cons 4 7) (cons 6 9) (cons 8 10)))
