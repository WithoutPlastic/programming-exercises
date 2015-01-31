#lang racket

(rule (end-in-grandson (grandson)))
(rule (end-in-grandson (?x . ?rest))
      (end-in-grandson ?rest))

(rule ((grandson) ?x ?y)
      (grandson ?x ?y))

(rule ((great . ?rel) ?x ?y)
      (and (end-in-grandson ?rel)
           (son ?x ?z)
           (?rel ?z ?y)))
