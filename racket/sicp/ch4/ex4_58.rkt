#lang racket

;Answer:
;'(assert! (rule (bit-shot ?name ?division)
;                (and (job ?name (?division . ?title))
;                     (supervisor ?name ?supervisor)
;                     (job ?supervisor (?supervisor-division . ?supervisor-title))
;                     (not (same ?division ?supervisor-division)))))
