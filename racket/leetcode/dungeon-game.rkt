#lang racket

;Problem:
;The demons had captured the princess (P) and imprisoned her in the bottom-right
;corner of a dungeon. The dungeon consists of M x N rooms laid out in a 2D grid.
;Our valiant knight (K) was initially positioned in the top-left room and must
;fight his way through the dungeon to rescue the princess.
;
;The knight has an initial health point represented by a positive integer. If at
;any point his health point drops to 0 or below, he dies immediately.
;
;Some of the rooms are guarded by demons, so the knight loses health (negative
;integers) upon entering these rooms; other rooms are either empty (0's) or
;contain magic orbs that increase the knight's health (positive integers).
;
;In order to reach the princess as quickly as possible, the knight decides to
;move only rightward or downward in each step.
;
;Write a function to determine the knight's minimum initial health so that he
;is able to rescue the princess.
;
;For example, given the dungeon below, the initial health of the knight must
;be at least 7 if he follows the optimal path RIGHT-> RIGHT -> DOWN -> DOWN.
;
;  -2 (K)  -3      3
;  -5      -10     1
;  10      30      -5 (P)
;
;Notes:
;
; - The knight's health has no upper bound.
; - Any room can contain threats or power-ups, even the first room the knight
;   enters and the bottom-right room where the princess is imprisoned.
;
;Credits:
;Special thanks to @stellari for adding this problem and creating all test
;cases.

(require "lib/permutation.rkt")

(define [calculate-minimum-hp dungeon]
  (define down (λ [pos] (cons (add1 (car pos)) (cdr pos))))
  (define right (λ [pos] (cons (car pos) (add1 (cdr pos)))))
  (define [matrix-ref m p] (list-ref (list-ref m (car p)) (cdr p)))
  (define [gen-paths p ops]
    (if [null? ops] (list p) (cons p (gen-paths ((car ops) p) (cdr ops)))))
  (define [path-min-health-requirement path]
    (define [iter last-health remaining min-health]
      (if [null? remaining] min-health
        (let* ([cur-health (+ last-health (car remaining))]
               [updated-min-health (max (add1 (- cur-health)) min-health)])
          (iter cur-health (cdr remaining) updated-min-health))))

    (iter 0 (map (curry matrix-ref dungeon) path) 1))

  (let* ([row (length dungeon)] [col (length (car dungeon))]
         [ops (append (make-list (sub1 row) down) (make-list (sub1 col) right))]
         [paths (map (curry gen-paths '(0 . 0)) (unique-permute ops))])
    (apply min (map path-min-health-requirement paths))))

(calculate-minimum-hp (list '(-2  -3  3) '(-5  -10 1) '(10  30  -5)))
