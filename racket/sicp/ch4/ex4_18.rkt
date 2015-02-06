#lang racket

;Answer: The solution in exercise not work. And it just met the specific case.
;The default value is refered for evalution, which caused a type error.
;But the previous solution is fine. the first reference is delayed. When first
;element is refered, it just setted well.
