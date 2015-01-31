#lang racket

(rule (outranked-by ?staff-person ?boss)
      (or (supervisor ?staff-person ?boss)
          (and (outranked-by ?middle-manager ?boss)
               (supervisor ?staff-person ?middle-manager))))

;< (outranked-by (Bitdiddle Ben) ?who)
;
;Answer: At the first sight of the new outranked-by, it doesn't met
;tail-recursive format. So according the order of evaluation, and/or, a infinity
;loop happened.
