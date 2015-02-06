#lang racket

;Answer: Acutally in text book, we can rebind symbol to a user defined procedure,
; and all its argument evaluation is lazy. And them we can try to define a
; concept but don't fully evaluate it(which may cost infinity hardware
; resource).
; Another benefit is simple syntax, can be mixed with high-level procedure.
