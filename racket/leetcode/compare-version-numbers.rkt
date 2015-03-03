#lang racket

;Problem:
;Compare two version numbers version1 and version2.
;If version1 > version2 return 1, if version1 < version2 return -1, otherwise
;return 0.
;
;You may assume that the version strings are non-empty and contain only digits
;and the . character. The . character does not represent a decimal point and is
;used to separate number sequences.
;
;For instance, 2.5 is not "two and a half" or "half way to version three", it
;is the fifth second-level revision of the second first-level revision.
;
;Here is an example of version numbers ordering:
;
;  0.1 < 1.1 < 1.2 < 13.37
;
;Credits:
;Special thanks to @ts for adding this problem and creating all test cases.""""

(require "lib/char-number-convert.rkt")

(define [compare-version v-a v-b]
  (define delimiter ".")

  (let* ([string->number (compose chars->number string->list)]
         [version-a (map string->number (string-split v-a delimiter))]
         [version-b (map string->number (string-split v-b delimiter))])
    (define [subcompare remaining-a remaining-b]
      (if [and [null? remaining-a] [null? remaining-b]] 0
        (let ([first-a (car remaining-a)] [first-b (car remaining-b)])
          (cond ([< first-b first-a] 1)
                ([< first-a first-b] -1)
                (else (subcompare (cdr remaining-a) (cdr remaining-b)))))))

    (subcompare version-a version-b)))

(compare-version "0.1" "1.1")
(compare-version "1.1" "1.1")
(compare-version "1.1" "2.0")
