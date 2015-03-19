#lang racket

(require "./int-bytes-convert.rkt")

(define [sort-in-file in-file out-file mem-limit tmp-path compare]
  (let ([total-size (file-size in-file)]
        [in-temp (curry string-append tmp-path)]
        [bytes-compare (if [eq? compare <] bytes<? bytes>?)])
    (define [sort-in-memory]
      (displayln 'sort-in-memory)
      (define [read-file-4-byte-chunk]
        (call-with-input-file
          in-file
          (λ [i] (let iter ([readed (read-bytes 4 i)] [accum '()])
                   (if [eof-object? readed] (reverse accum)
                     (iter (read-bytes 4 i) (cons readed accum)))))))

      (let* ([4bytes-lst (read-file-4-byte-chunk)]
             [sorted (sort 4bytes-lst bytes-compare)]
             [out (open-output-file out-file #:exists 'truncate)])
        (for-each (λ [chunk] (write-bytes chunk out)) sorted)
        (close-output-port out)))

    (define [split-and-merge]
      (define [split]
        (displayln 'split)
        (let* ([4bytes-size (/ total-size 4)]
               [h-halve-size (* (floor (/ 4bytes-size 2)) 4)]
               [t-halve-size (* (- 4bytes-size (floor (/ 4bytes-size 2))) 4)]
               [out-name (λ [suffix] (in-temp (string-append (number->string 4bytes-size) suffix)))]
               [out-h-file (out-name "h")]
               [out-t-file (out-name "t")]
               [in (open-input-file in-file)]
               [out-h (open-output-file out-h-file #:exists 'truncate)]
               [out-t (open-output-file out-t-file #:exists 'truncate)])
          (write-bytes (read-bytes h-halve-size in) out-h)
          (write-bytes (read-bytes t-halve-size in) out-t)

          (close-output-port out-h)
          (close-output-port out-t)
          (close-input-port in)

          (sort-in-file out-h-file out-h-file mem-limit tmp-path compare)
          (sort-in-file out-t-file out-t-file mem-limit tmp-path compare)
          (values out-h-file out-t-file)))

      (define [merge h-file t-file]
        (displayln 'merge)
        (let ([in-h (open-input-file h-file)]
              [in-t (open-input-file t-file)]
              [out (open-output-file out-file #:exists 'truncate)])
          (let iter ([readed-h (read-bytes 4 in-h)] [readed-t (read-bytes 4 in-t)])
            (cond ([eof-object? readed-h]
                   (write-bytes readed-t out)
                   (write-bytes (port->bytes in-t) out))
                  ([eof-object? readed-t]
                   (write-bytes readed-h out)
                   (write-bytes (port->bytes in-h) out))
                  ([bytes-compare readed-h readed-t]
                   (write-bytes readed-h out)
                   (iter (read-bytes 4 in-h) readed-t))
                  (else (write-bytes readed-t out)
                        (iter readed-h (read-bytes 4 in-t)))))

          (close-input-port in-h)
          (close-input-port in-t)
          (close-output-port out)))

      (call-with-values (λ [] (split)) merge))

    (if [<= total-size mem-limit] (sort-in-memory) (split-and-merge))))

(provide sort-in-file)

;(let ([input-data-file "input.data"]
;      [output-data-file "output.data"]
;      [temp-file-path "tmp/"])
;  (when [file-exists? input-data-file]
;    (unless [directory-exists? temp-file-path] (make-directory temp-file-path))
;    (sort-in-file input-data-file output-data-file 10000000 temp-file-path <)
;    (delete-directory/files temp-file-path)))
