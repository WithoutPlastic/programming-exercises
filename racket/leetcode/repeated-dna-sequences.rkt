#lang racket

;Problem:
;All DNA is composed of a series of nucleotides abbreviated as A, C, G, and T,
;for example: "ACGAATTCCG". When studying DNA, it is sometimes useful to
;identify repeated sequences within the DNA.
;
;Write a function to find all the 10-letter-long sequences (substrings) that
;occur more than once in a DNA molecule.
;
;For example,
;
;Given s = "AAAAACCCCCAAAAACCCCCCAAAAAGGGTTT",
;
;Return: ["AAAAACCCCC", "CCCCCAAAAA"].

(define [find-repeated-dna-sequences str repeat-len]
  (let ([chars (string->list str)] [len (string-length str)])
    (if [<= len repeat-len] '()
      (let* ([take-by-offset (λ [i] (take (drop chars i) repeat-len))]
             [slices (map take-by-offset (range 0 (add1 (- len repeat-len))))])
        (filter-map (λ [s] [and [< 1 (count (curry equal? s) slices)]
                                (list->string s)])
                    (remove-duplicates slices))))))

(define test-str "AAAAACCCCCAAAAACCCCCCAAAAAGGGTTT")

(find-repeated-dna-sequences test-str 10)
