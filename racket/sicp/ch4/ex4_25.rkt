#lang racket

;Answer: In application order scheme, not do cond verdict, the factorial
;calculation will continue, path looks like n, n-1, n-2, ... , 1, 0, -1, -2.
;And never ended. So application order not work. But turn to normal order, it
;is normal case, ended at n equal to 1. It is lazy, never do unnecessary work.
