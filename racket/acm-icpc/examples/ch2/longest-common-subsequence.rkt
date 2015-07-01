#lang racket

;Problem:
;Given two string S and T with length n. Find out longest common subsequence.
;
;The subsequence met the requirement: S1 S2 ... Sn => Si1 Si2 ... Sim (i1 < i2 < ... < im).
;
;Restriction:
;1 <= n, m <= 1000


(require "../../lib/memorize-function.rkt")


(define [find-longest-common-subsequence str-s str-t]
  (let ([char-lst-s (string->list str-s)]
        [char-lst-t (string->list str-t)])
    (let iter ([common-len 0] [char-lst-s char-lst-s] [char-lst-t char-lst-t])
      (if [or [null? char-lst-s] [null? char-lst-t]] common-len
        (let ([last-char-s (last char-lst-s)]
              [last-char-t (last char-lst-t)])
          (if [not [eq? last-char-s last-char-t]]
            (iter (add1 common-len) (drop-right char-lst-s 1) (drop-right char-lst-t 1))
            (max (iter common-len (drop-right char-lst-s 1) char-lst-t)
                 (iter common-len char-lst-s (drop-right char-lst-t 1)))))))))


((memorize-func find-longest-common-subsequence) "abcd" "becd")
