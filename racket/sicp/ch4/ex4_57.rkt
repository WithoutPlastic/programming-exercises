#lang racket

;Answer:
;a> '(assert (rule (can-replace ?person-a ?person-b)
;                  (and (job ?person-a ?job-a)
;                       (job ?person-b ?job-b)
;                       (not (same ?person-a ?person-b))
;                       (or (same ?job-a ?job-b)
;                           (can-do-job ?job-a ?job-b)))))
;b> '(can-replace ?who (Fect Cy D))
;c> '(and (can-replace ?person-a ?person-b)
;         (salary ?person-a ?salary-a)
;         (salary ?person-b ?salary-b)
;         (lisp-value < ?salary-a ?salary-b))
