#lang racket

(define input-data-file "input.data")
(define output-data-file "output.data")
(define temp-file-path "tmp/")
(define in-temp (curry string-append temp-file-path))
(define memory-limit-in-line 5000)

(define [get-input-integers-number file-path]
  (call-with-input-file
    file-path
    (λ [in] (let iter ([cnt 0])
                 (if [eof-object? (read-line in)] cnt (iter (add1 cnt)))))))

(define [split-file file-path offset]
  (let* ([in (open-input-file file-path)]
         [output-prefix (number->string offset)]
         [output-head-file (in-temp (string-append output-prefix "h"))]
         [output-tail-file (in-temp (string-append output-prefix "t"))]
         [out-h (open-output-file output-head-file #:exists 'truncate)]
         [out-t (open-output-file output-tail-file #:exists 'truncate)])
    (let write-first-half ([remaining offset])
      (if [<= remaining 0] (close-output-port out-h)
        (begin
          (displayln (read-line in) out-h)
          (write-first-half (sub1 remaining)))))

    (let write-left ()
      (let ([readed (read-line in)])
        (if [eof-object? readed] (close-output-port out-t)
          (begin
            (displayln readed out-t)
            (write-left)))))

    (close-input-port in)
    (values output-head-file output-tail-file)))

(define [sort-in-memory input-file-path output-file-path compare]
  (display-lines-to-file
    (map number->string (sort (map string->number (file->lines input-file-path)) compare))
    output-file-path
    #:exists 'truncate))

(define [sort-in-file input-file-path output-file-path compare]
  (let* ([line-count (get-input-integers-number input-file-path)]
         [head-half-count (floor (/ line-count 2))]
         [tail-half-count (- line-count head-half-count)])
    (define [merge input-file-path-a input-file-path-b]
      (let ([in-a (open-input-file input-file-path-a)]
            [in-b (open-input-file input-file-path-b)]
            [out (open-output-file output-file-path #:exists 'truncate)])
        (let iter ([readed-a (read-line in-a)] [readed-b (read-line in-b)])
          (unless [and [eof-object? readed-a] [eof-object? readed-b]]
            (cond ([eof-object? readed-a]
                   (displayln readed-b out)
                   (iter eof (read-line in-b)))
                  ([eof-object? readed-b]
                   (displayln readed-a out)
                   (iter (read-line in-a) eof))
                  ([compare (string->number readed-a) (string->number readed-b)]
                   (displayln readed-a out)
                   (iter (read-line in-a) readed-b))
                  (else (displayln readed-b out)
                        (iter readed-a (read-line in-b))))))

        (close-input-port in-a)
        (close-input-port in-b)
        (close-output-port out)))

    (define [split-and-merge]
      (let-values ([(h-file t-file) (split-file input-file-path head-half-count)])
        (sort-in-file h-file h-file compare)
        (sort-in-file t-file t-file compare)
        (merge h-file t-file)))

    (if [< memory-limit-in-line line-count] (split-and-merge)
      (sort-in-memory input-file-path output-file-path compare))))

(when [file-exists? input-data-file]
  (unless [directory-exists? temp-file-path] (make-directory temp-file-path))
  (sort-in-file input-data-file output-data-file <)
  (delete-directory/files temp-file-path))
