#lang racket

;Answer:
;a> (and (supervisor ?person (Bitdiddle Ben))
;        (address ?person ?where))
;b> (and (salary (Bitdiddle Ben) ?number)
;        (salary ?person ?amount)
;        (lisp-value < ?amount ?number))
;c> (and (supervisor ?person ?boss)
;        (job ?boss ?job)
;        (not (job ?boss (computer . ?type))))
