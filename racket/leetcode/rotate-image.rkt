#lang racket

;Probelm:
;You are given an n x n 2D matrix representing an image.
;
;Rotate the image by 90 degrees (clockwise).
;
;Follow up:
;Could you do this in-place?

(define [rotate matrix] (map reverse (apply map list matrix)))

(define test-matrix (list '(1 2 3) '(4 5 6) '(7 8 9)))

(rotate test-matrix)
