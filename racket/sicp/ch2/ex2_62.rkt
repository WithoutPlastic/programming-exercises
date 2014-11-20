#lang racket

(define [element-of-set? element set]
  (cond ([null? set] #f)
        ([= element (car set)] #t)
        ([< x (car set)] #f)
        (else (element-of-set? x (cdr set))))

(define [adjoin-set element set]
  (cond ([null? set] (cons element set))
        ([< element (car set)] (cons element set))
        ([= element (car set)] set)
        ([< (car set) element] (cons (car set) (adjoin-set element (cdr set))))))

(define [intersection-set set-a set-b]
  (if [or (null? set-a) (null? set-b)] '()
    (let ([a-head (car set-a)]
          [b-head (car set-b)])
      (cond ([= a-head b-head]
             (cons a-head (intersection-set (cdr set-a) (cdr set-b))))
            ([< a-head b-head]
             (intersection-set (cdr set-a) set-b))
            ([< b-head a-head]
             (intersection-set set-a (cdr set-b)))))))

(define [union-set set-a set-b]
  (cond ([null? set-a] set-b)
        ([null? set-b] set-a)
        (else
          (let ([a-head (car set-a)]
                [b-head (car set-b)])
            (cond ([= a-head b-head]
                   (cons a-head
                         (union-set (cdr set-a)
                                    (cdr set-b))))
                  ([< a-head b-head]
                   (cons a-head
                         (union-set (cdr set-a)
                                    set-b)))
                  ([< b-head a-head]
                   (cons b-head
                         (union-set set-a
                                    (cdr set-b)))))))))
