#lang racket

;Answer: The order of requirements will not affect results, but will affect the
;time taken to get the results. It is obvious. An basic solution to reduce
;overhead is put low overhead and more retrictful requirement ahead. For those
;high overhead/more retrictful and low overhead/less retrictful combinations,
;we should do tradeoff based on quantitative benchmark.
;A generic analysis:
;Assume there is ordered requirements R0, R1, R2, each overhead T0, T1, T2.
;And initial combinations count be C0, and after requirement be C1, C2, C3.
;Simple graph:
;
;C0 -----> C1 -----> C2 -----> C3
;     R0        R1        R2
;  C0 * T0   C1 * T1   C2 * T2
;Total overhead: C0 * T0 + C1 * T1 + C2 * T2
