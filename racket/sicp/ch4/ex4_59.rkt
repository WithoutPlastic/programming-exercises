#lang racket

;(meeting accounting (Monday 9am))
;(meeting administration (Monday 10am))
;(meeting computer (Wednesday 3pm))
;(meeting administration (Friday 1pm))
;(meeting whole-company (Wednesday 4pm))

;Answer:
;a> '(meeting ?division (Friday ?schedule))
;b> '(assert! (rule (meeting-date ?person ?date)
;                   (or (and (job ?person (?division . ?title))
;                            (meeting ?division ?date))
;                       (meeting whole-company ?date))))
;c> '(meeting-date (Hacker Alyssa P) (Wednesday ?date))
