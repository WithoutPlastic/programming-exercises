#lang racket

;Answer: the extra frame is come from the lambda call, which will call
;extend-environment once. And there is no different obviously. For
;simultaneously exvalution, the SICP already hint us, to pull ahead all
;definitions, like those classical language.
