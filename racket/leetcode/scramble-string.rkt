#lang racket

;Problem:
;Given a string s1, we may represent it as a binary tree by partitioning it to
;two non-empty substrings recursively.
;
;Below is one possible representation of s1 = "great":
;
;        great
;       /    \
;      gr    eat
;     / \    /  \
;    g   r  e   at
;               / \
;              a   t
;
;To scramble the string, we may choose any non-leaf node and swap its two
;children.
;
;For example, if we choose the node "gr" and swap its two children, it produces
;a scrambled string "rgeat".
;
;          rgeat
;         /    \
;        rg    eat
;       / \    /  \
;      r   g  e   at
;                 / \
;                a   t
;We say that "rgeat" is a scrambled string of "great".
;
;Similarly, if we continue to swap the children of nodes "eat" and "at", it
;produces a scrambled string "rgtae".
;
;       rgtae
;       /    \
;      rg   tae
;     / \   /  \
;    r   g ta  e
;          / \
;         t   a
;
;We say that "rgtae" is a scrambled string of "great".
;
;Given two strings s1 and s2 of the same length, determine if s2 is a scrambled
;string of s1.

(require "lib/binary-tree.rkt")

(define [scramble? root-a root-b]
  (define [iter bnode-a bnode-b]
    (define [continue len]
      (let* ([la (bnode-left bnode-a)] [ra (bnode-right bnode-a)]
             [lb (bnode-left bnode-b)] [rb (bnode-right bnode-b)]
             [normal-iter (λ _ [and (iter la lb) (iter ra rb)])]
             [swap-iter (λ _ [and (iter la rb) (iter ra lb)])])
        (cond ([even? len] [or (normal-iter) (swap-iter)])
              ([= (string-length (bnode-payload la))
                  (string-length (bnode-payload lb))]
               (normal-iter))
              (else (swap-iter)))))

    (let ([payload-a (bnode-payload bnode-a)]
          [payload-b (bnode-payload bnode-b)])
      (if [and [bnode-last? bnode-a] [bnode-last? bnode-b]]
        [string=? payload-a payload-b]
        (continue (string-length payload-a)))))

  (iter root-a root-b))

(define-values (mk-bn mk-ban) (values make-btree-node make-btree-alone-node))
(define test-a
  (mk-bn "great"
         (mk-bn "gr" (mk-ban "g") (mk-ban "r"))
         (mk-bn "eat" (mk-ban "e") (mk-bn "at" (mk-ban "a") (mk-ban "t")))))
(define test-b
  (mk-bn "rgeat"
         (mk-bn "rg" (mk-ban "r") (mk-ban "g"))
         (mk-bn "eat" (mk-ban "e") (mk-bn "at" (mk-ban "a") (mk-ban "t")))))
(define test-c
  (mk-bn "rgtae"
         (mk-bn "rg" (mk-ban "r") (mk-ban "g"))
         (mk-bn "tae" (mk-bn "ta" (mk-ban "t") (mk-ban "a")) (mk-ban "e"))))
(define test-d
  (mk-bn "rgate"
         (mk-bn "rg" (mk-ban "r") (mk-ban "g"))
         (mk-bn "ate" (mk-ban "a") (mk-bn "te" (mk-ban "t") (mk-ban "e")))))

[scramble? test-a test-b]
[scramble? test-a test-c]
[scramble? test-b test-c]
[scramble? test-a test-d]
[scramble? test-c test-d]
