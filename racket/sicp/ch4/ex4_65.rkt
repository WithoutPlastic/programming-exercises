#lang racket

(rule (wheel ?person)
      (and (supervisor ?middle-manager ?person)
           (supervisor ?x ?middle-manager)))

;Answer: since the wheel rule show all person that match pattern. So same person
;appeared multitimes means match happened multi times. And the Warbucks Oliver
;accutally be the one matched four times. 
