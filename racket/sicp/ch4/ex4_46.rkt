#lang racket

;Answer: If left to right arguments evaluation successed, reverse failed. The
;only explanation is the dependency. Some state is changed, and the order
;matters. In this case, the parse-sentence, it will change the *unparsed*
;variable after success parse noun followed by parse-verb. By in another
;direction, parse verb first will cause error because the ahead noun is not
;removed.
