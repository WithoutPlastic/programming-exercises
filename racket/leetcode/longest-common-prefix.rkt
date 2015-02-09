#lang racket

;Problem
;Write a function to find the longest common prefix string amongst an array of
;strings.

(define [find-longest-common-prefix-strings strings]
  (let ([chars-list (map string->list strings)])
    (define [iter remainings idx]
      (define [split-common strs]
        (define [continue]
          (let* ([first-str (car strs)]
                 [fstr-fchar (list-ref first-str idx)]
                 [same-prefix? (lambda [x] [eq? (list-ref x idx) fstr-fchar])]
                 [same-fchar-strs (filter same-prefix? (cdr strs))]
                 [others-strs (filter-not same-prefix? (cdr strs))])
            (if [null? same-fchar-strs]
              (split-common others-strs)
              (cons (cons first-str same-fchar-strs)
                    (split-common others-strs)))))

        (if [null? strs] '() (continue)))

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
