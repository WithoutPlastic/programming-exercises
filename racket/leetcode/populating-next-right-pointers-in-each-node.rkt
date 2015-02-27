#lang racket

;Problem:
;Given a binary tree
;
;  struct TreeLinkNode {
;    TreeLinkNode *left;
;    TreeLinkNode *right;
;    TreeLinkNode *next;
;  }
;
;Populate each next pointer to point to its next right node. If there is no next
;right node, the next pointer should be set to NULL.
;
;Initially, all next pointers are set to NULL.
;
;Note:
; - You may only use constant extra space.
; - You may assume that it is a perfect binary tree
; (ie, all leaves are at the same level, and every parent has two children).
;
;For example,
;Given the following perfect binary tree,
;
;        1
;       / \
;      2   3
;     / \ / \
;    4  5 6  7
;
;After calling your function, the tree should look like:
;
;         1 -> NULL
;       /  \
;      2 -> 3 -> NULL
;     / \  / \
;    4->5->6->7 -> NULL

(require "lib/triple-tree.rkt")

(define [connect! root]
  (define [tri-tree-side-nodes root left-or-right]
    (define [iter tnode]
      (if [null? tnode] '() (cons tnode (iter (left-or-right tnode)))))

    (iter root))
  (define tri-tree-lefts (curryr tri-tree-side-nodes tnode-left))
  (define tri-tree-rights (curryr tri-tree-side-nodes tnode-right))

  (unless [null? root]
    (let* ([left-tnode (tnode-left root)]
           [right-tnode (tnode-right root)]
           [l-rights (tri-tree-rights left-tnode)]
           [r-lefts (tri-tree-lefts right-tnode)])
      (map tnode-set-next! l-rights r-lefts)
      (connect! left-tnode)
      (connect! right-tnode))))

(define test-left-br
  (make-tnode 2 '() (make-tnode 4 '() '() '()) (make-tnode 5 '() '() '())))
(define test-right-br
  (make-tnode 3 '() (make-tnode 6 '() '() '()) (make-tnode 7 '() '() '())))
(define test-tri-tree (make-tnode 1 '() test-left-br test-right-br))

(connect! test-tri-tree)
test-tri-tree
