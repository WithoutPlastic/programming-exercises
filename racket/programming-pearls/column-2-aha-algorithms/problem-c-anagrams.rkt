#lang racket

(define [group-anagrams words]
  (define [group word-pair-lst]
    (let iter ([remaining word-pair-lst] [accum '()] [result '()] [c13n (caar word-pair-lst)])
      (cond ([null? remaining] (cons accum result))
            ([equal? (caar remaining) c13n]
             (iter (cdr remaining) (cons (car remaining) accum) result c13n))
            (else (iter remaining '() (cons accum result) (caar remaining))))))

  (map (curry map cdr)
       (group (sort (map (λ [w] (cons (list->string (sort (string->list w) char<?)) w)) words)
                    (λ [a b] [string<? (car a) (car b)])))))

(group-anagrams (list "pans" "pots" "opt" "snap" "stop" "tops"))
