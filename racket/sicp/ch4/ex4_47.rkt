#lang racket

;Answer: It doesn't work correctly. The parser will product the correct answer
;when run for the first time. But if further try-again performed, it will stuck
;in a infinite loop duw to alway failing (parse-prepositional-phase). The
;failure will cause the evaluation (list 'verb-phase (parse-verb-phase)
;(parse-prepositional-phase)), which in turn evaluation
;(parse-prepositional-phase) again.
;
;If a misspell word presented, parser will not work too.
;
;Change the argument evaluation order will make it failure at the first time.
