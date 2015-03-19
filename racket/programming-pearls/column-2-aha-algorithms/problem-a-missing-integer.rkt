#lang racket

(require "../column-1-cracking-the-oyster/sort-integers-in-file.rkt")
(require "../column-1-cracking-the-oyster/int-bytes-convert.rkt")

(define read-4-bytes (curry read-bytes 4))

(define [create-sorted-file in-file out-file tmp-file limit]
  (when [file-exists? in-file]
    (unless [directory-exists? tmp-file] (make-directory tmp-file))
    (sort-in-file in-file out-file limit tmp-file <)
    (delete-directory/files tmp-file)))

(define [binary-search-missing in-file]
  (let iter ([offset-in-4 0] [range-in-4 (/ (file-size in-file) 4)])
    (cond ([= range-in-4 1]
           (call-with-input-file in-file
             (λ [i] (file-position i (* offset-in-4 4))
                (let ([first-n (be-4bytes->int (read-4-bytes i))]
                      [second-n (be-4bytes->int (read-4-bytes i))]
                      [third-n (be-4bytes->int (read-4-bytes i))])
                  (cond ([< offset-in-4 first-n] offset-in-4)
                        ([< (add1 offset-in-4 first-n) (add1 offset-in-4)])
                        (else (+ offset-in-4 2)))))))
          ([= range-in-4 2]
           (call-with-input-file in-file
             (λ [i] (file-position i (* offset-in-4 4))
                (let ([first-n (be-4bytes->int (read-4-bytes i))]
                      [second-n (be-4bytes->int (read-4-bytes i))]
                      [third-n (be-4bytes->int (read-4-bytes i))]
                      [fourth-n (be-4bytes->int (read-4-bytes i))])
                  (cond ([< offset-in-4 first-n] offset-in-4)
                        ([< (+ offset-in-4 1) second-n] (+ offset-in-4 1))
                        ([< (+ offset-in-4 2) third-n] (+ offset-in-4 2))
                        (else (+ offset-in-4 3)))))))
          (else (let* ([h-range-in-4 (floor (/ range-in-4 2))]
                       [t-range-in-4 (- range-in-4 h-range-in-4)]
                       [probe-idx (+ offset-in-4 h-range-in-4)]
                       [in (open-input-file in-file)]
                       [_ (file-position in (* probe-idx 4))]
                       [readed (be-4bytes->int (read-4-bytes in))])
                  (close-input-port in)
                  (if [= readed probe-idx]
                    (iter (add1 probe-idx) t-range-in-4)
                    (iter offset-in-4 h-range-in-4)))))))

(let ([sorted-data-file "sorted.data"])
  (create-sorted-file "one-missing.data" sorted-data-file "tmp/" 10000000)
  (binary-search-missing sorted-data-file))
