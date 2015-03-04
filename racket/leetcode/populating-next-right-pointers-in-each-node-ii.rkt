#lang racket

;Problem:
;Follow up for problem "Populating Next Right Pointers in Each Node".
;
;What if the given tree could be any binary tree? Would your previous solution
;still work?
;
;Note:
; - You may only use constant extra space.
;
;For example,
;Given the following binary tree,
;
;         1
;       /  \
;      2    3
;     / \    \
;    4   5    7
;
;After calling your function, the tree should look like:
;
;         1 -> NULL
;       /  \
;      2 -> 3 -> NULL
;     / \    \
;    4-> 5 -> 7 -> NULL

(require "lib/triple-tree.rkt")

(define [connect! root]
  (define [connect-left-to-right! l-tnode r-tnode]
    (when [nor [null? l-tnode] [null? r-tnode]]
      (tnode-set-next! l-tnode r-tnode)
      (let ([ll-tnode (tnode-left l-tnode)] [lr-tnode (tnode-right l-tnode)]
            [rl-tnode (tnode-left r-tnode)] [rr-tnode (tnode-right r-tnode)])
        (connect-left-to-right!
          (if [null? lr-tnode] ll-tnode lr-tnode)
          (if [null? rl-tnode] rr-tnode rl-tnode)))))

  (unless [null? root]
    (let ([left-tnode (tnode-left root)] [right-tnode (tnode-right root)])
      (connect-left-to-right! left-tnode right-tnode)
      (connect! left-tnode)
      (connect! right-tnode))))

(define test-left
  (make-tnode 2 '() (make-tnode 4 '() '() '()) (make-tnode 5 '() '() '())))
(define test-right (make-tnode 3 '() '() (make-tnode 7 '() '() '())))
(define test-tree (make-tnode 1 '() test-left test-right))

(connect! test-tree)
test-tree
