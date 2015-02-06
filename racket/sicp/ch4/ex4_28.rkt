#lang racket

;Answer: Actually, the when call a compound procedure, all parameter is
;delayed via normal eval entrance. So if the delay one is a procedure, you must
;force it to the value, then the call can continue as expect.
