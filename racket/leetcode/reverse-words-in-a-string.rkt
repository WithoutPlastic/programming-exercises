#lang racket

;Problem:
;Given an input string, reverse the string word by word.
;
;For example,
;Given s = "the sky is blue",
;return "blue is sky the".
;
;Update (2015-02-12):
;For C programmers: Try to solve it in-place in O(1) space.
;
;Clarification:
;
; - What constitutes a word?
; - A sequence of non-space characters constitutes a word.
; - Could the input string contain leading or trailing spaces?
; - Yes. However, your reversed string should not contain leading or trailing spaces.
; - How about multiple spaces between two words?
; - Reduce them to a single space in the reversed string.

(define [reverse-words str]
  (define space #\space)
  (define space? (curry eq? space))

  (define [trim/merge-space chars]
    (define [iter remaining]
      (cond ([null? remaining] '())
            ([= (length remaining) 1]
             (if [space? (car remaining)] '() remaining))
            (else (let ([fc (car remaining)]
                        [sc (cadr remaining)]
                        [rests (cdr remaining)])
                    (if [and [space? fc] [space? sc]]
                      (iter rests) (cons fc (iter rests)))))))

    (cond ([null? chars] '())
          ([space? (car chars)] (trim/merge-space (cdr chars)))
          (else (iter chars))))

  (define [reverse-list chars]
    (if [< (length chars) 2] chars
      (append (list (last chars))
              (reverse-list (drop-right (cdr chars) 1))
              (list (first chars)))))

  (define [reverse-word chars]
    (define [split-at-space w-chars remaining]
      (cond ([null? remaining] (values w-chars remaining))
            ([space? (car remaining)] (values w-chars (cdr remaining)))
            (else (split-at-space (append w-chars (list (car remaining)))
                                  (cdr remaining)))))

    (if [null? chars] '()
      (call-with-values
        (thunk (split-at-space '() chars))
        (λ [w r] (append (reverse-list w)
                         (if [null? r] '() (cons space (reverse-word r))))))))

  (list->string
    (reverse-word (reverse-list (trim/merge-space (string->list str))))))
(provide reverse-words)

(define test-str "  the sky   is blue ")

(reverse-words test-str)
