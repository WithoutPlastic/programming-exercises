#lang racket

;Problem:
;Implement an iterator over a binary search tree (BST). Your iterator will be
;initialized with the root node of a BST.
;
;Calling next() will return the next smallest number in the BST.
;
;Note: next() and hasNext() should run in average O(1) time and uses O(h)
;memory, where h is the height of the tree.
;
;Credits:
;Special thanks to @ts for adding this problem and creating all test cases.

(require "lib/binary-tree.rkt")

(define bst-iterator% (class object%
  (init root) (super-new)
  (define end-symbol 'end)
  (define [generate-iterator-entrys root back]
    (if [null? root] back
      (generate-iterator-entrys
        (bnode-left root)
        (thunk (values (bnode-payload root)
                       (generate-iterator-entrys (bnode-right root) back))))))
  (define entry (generate-iterator-entrys root end-symbol))
  (define/public [next!]
    (let-values ([(val next-entry) (entry)]) (set! entry next-entry) val))
  (define/public [last?] [eq? entry end-symbol])))

(define iterator
  (new bst-iterator% [root (btree-parse '(5 1 8 0 3 7 10 - - - - 6 - - 11))]))

((λ [f n] (for-each (λ [f] (f)) (make-list n f)))
 (thunk (displayln (send iterator next!)) (displayln (send iterator last?))) 10)
