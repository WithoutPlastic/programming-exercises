#lang racket

;Problem:
;There is NxM maze.
;Symbol % means wall which you can't pass through.
;Symbol - means space which can be pass through.
;Symbol S means start point.
;Symbol G means end point.
;You can only move up/down/left/right to neighbor space.
;
;Write down a program to calculate the shortest step from start point to end point.
;
;Restriction:
;N <= 100
;M <= 100

(define End 'G)
(define Start 'S)
(define Wall '%)
(define Space '-)


(define [matrix-index-of-elt mat elt]
  (let ([row-len (length mat)]
        [col-len (length (car mat))])
    (let row-iter ([row-idx 0])
      (and [< row-idx row-len]
           (let col-iter ([col-idx 0])
             (cond ([<= col-len col-idx] (row-iter (add1 row-idx)))
                   ([eq? (list-ref (list-ref mat row-idx) col-idx) elt] (cons row-idx col-idx))
                   (else (col-iter (add1 col-idx)))))))))


(define [matrix-ref mat pos] (list-ref (list-ref mat (car pos)) (cdr pos)))


(define [neighbor-pos row-len col-len pos]
  (filter (λ [pos] [and [<= 0 (car pos)] [< (car pos) row-len]
                        [<= 0 (cdr pos)] [< (cdr pos) col-len]])
          (list (cons (add1 (car pos)) (cdr pos)) (cons (sub1 (car pos)) (cdr pos))
                (cons (car pos) (add1 (cdr pos))) (cons (car pos) (sub1 (cdr pos))))))


(define [find-short-step-of-maze mat]
  (let ([row-len (length mat)]
        [col-len (length (car mat))]
        [start-pos (matrix-index-of-elt mat Start)]
        [end-pos (matrix-index-of-elt mat End)])
    (let iter ([step 0]
               [walked-pos-lst '()]
               [open-path-pos-lst (list start-pos)])
      (display 'walked:) (displayln walked-pos-lst)
      (display 'open-path:) (displayln open-path-pos-lst)
      (cond ([< 48 step] -1)
            ([null? open-path-pos-lst] -1)
            ([ormap (λ [pos] [eq? (matrix-ref mat pos) End]) open-path-pos-lst] step)
            (else
              (iter (add1 step)
                    (append open-path-pos-lst walked-pos-lst)
                    (append-map (λ [pos]
                                   (filter-not (curryr member walked-pos-lst)
                                               (filter-not (λ [pos] [eq? (matrix-ref mat pos) Wall])
                                                           (neighbor-pos row-len col-len pos))))
                                open-path-pos-lst)))))))

(find-short-step-of-maze
  (list '(% S % % % % % % - %)
        '(- - - - - - % - - %)
        '(- % - % % - % % - %)
        '(- % - - - - - - - -)
        '(% % - % % - % % % %)
        '(- - - - % - - - - %)
        '(- % % % % % % % - %)
        '(- - - - % - - - - -)
        '(- % % % % - % % % -)
        '(- - - - % - - - G %)))
