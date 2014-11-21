#lang racket

(define [lookup-tree given-key tree]
  (cond ([null? tree] #f)
        ([= given-key (key (entry tree))]
         (entry tree))
        ([< given-key (key (entry tree))]
         (lookup-tree given-key (left-branch tree)))
        ([< (key (entry tree)) given-key]
         (lookup-tree given-key (right-branch tree)))))
