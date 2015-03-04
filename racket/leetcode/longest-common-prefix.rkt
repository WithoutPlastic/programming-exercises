#lang racket

;Problem:
;Write a function to find the longest common prefix string amongst an array of
;strings.

(define [find-longest-common-prefix-strings strings]
  (let ([chars-list (map string->list strings)])
    (define [iter remainings idx]
      (define [split-common strs]
        (let* ([fchars (map car strs)]
               [unq-f-chars (remove-duplicates fchars)]
               [eq-fchar?s (map (lambda [x] (lambda [s] [eq? (car s) x]))
                                unq-f-chars)]
               [matched-groups (map (lambda [x] (filter x strs)) eq-fchar?s)])
          (filter (lambda [x] [< 1 (length x)]) matched-groups)))

      (let* ([len-filter (lambda [s] [< idx (length s)])]
             [l-remainings (map (lambda [x] (filter len-filter x)) remainings)])
        (define [continue]
          (let ([results (foldl append '() (map split-common l-remainings))])
            (if [null? results] remainings (iter results (add1 idx)))))

        (if [null? l-remainings] remainings (continue))))

    (map (lambda [x] (map list->string x))
         (iter (list chars-list) 0))))

(define test-strings-a
  (list "hello world!"
        "hello you americians!"
        "hello you chinese!"
        "hello we americians!"
        "hello we chinese!"
        "hello japanese!"
        "programmer should have no girlfriend!"
        "racket is simple language."
        "you are so beautiful!"
        ""))

(define test-strings-b
  (list "hello world!"
        "hello you americians!"
        "hello you chinese!"
        "hello we americians!"
        "hello we chinese!"
        "hello japanese!"
        "the best languages over world is php!"
        "the best languages over world is haskell!"
        "programmer should have no girlfriend!"
        "racket is simple language."
        "you are so beautiful!"
        ""))

(find-longest-common-prefix-strings test-strings-b)
