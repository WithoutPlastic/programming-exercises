#lang racket

;Problem:
;There is NxM matrix, symbol W means water, when eight neighbor not to be W. it count as one lake.
;Please counting how many lake in NxM matrix.
;
;Restriction:
;N <= 100
;M <= 100

(define water 'W)
(define non-water '-)


(define [matrix-index-of-elt mat elt]
  (let ([row-len (length mat)]
        [col-len (length (car mat))])
    (let row-iter ([row-idx 0])
      (and [< row-idx row-len]
           (let col-iter ([col-idx 0])
             (cond ([<= col-len col-idx] (row-iter (add1 row-idx)))
                   ([eq? (list-ref (list-ref mat row-idx) col-idx) elt] (cons row-idx col-idx))
                   (else (col-iter (add1 col-idx)))))))))


(define [neighbor-pos pos]
  (list (cons (add1 (car pos)) (cdr pos))
        (cons (sub1 (car pos)) (cdr pos))
        (cons (car pos) (add1 (cdr pos)))
        (cons (car pos) (sub1 (cdr pos)))
        (cons (add1 (car pos)) (add1 (cdr pos)))
        (cons (add1 (car pos)) (sub1 (cdr pos)))
        (cons (sub1 (car pos)) (add1 (cdr pos)))
        (cons (sub1 (car pos)) (sub1 (cdr pos)))))


(define [replace-by-pos mat pos]
  (let* ([row-idx (car pos)]
         [col-idx (cdr pos)]
         [head-mat (take mat row-idx)]
         [target-row (list-ref mat row-idx)]
         [tail-mat (drop mat (add1 row-idx))])
    (append head-mat
            (list (append (take target-row col-idx)
                          (list non-water)
                          (drop target-row (add1 col-idx))))
            tail-mat)))

(define [replace-by-pos-list mat pos-lst]
  (if [null? pos-lst] mat
    (replace-by-pos-list (replace-by-pos mat (car pos-lst)) (cdr pos-lst))))


(define [neighbor-water-pos-list mat pos]
  (let* ([row-len (length mat)]
         [col-len (length (car mat))]
         [neighbor-pos-lst (neighbor-pos pos)]
         [validate-pos (λ [pos] [and [< (car pos) row-len] [<= 0 (car pos)]
                                     [< (cdr pos) col-len] [<= 0 (cdr pos)]])]
         [valid-neighbor-pos-lst (filter validate-pos neighbor-pos-lst)])
    (filter (λ [pos] [eq? (list-ref (list-ref mat (car pos)) (cdr pos)) water])
            valid-neighbor-pos-lst)))


(define [lake-counting mat]
  (let iter ([cnt 0] [mat mat] [water-pos (matrix-index-of-elt mat water)])
    (displayln cnt)
    (if [false? water-pos] cnt
      (let reduce-lake ([mat (replace-by-pos mat water-pos)]
                        [pending-water-pos-lst (list water-pos)])
        (for-each displayln mat)
        (displayln '---)
        (if [null? pending-water-pos-lst] (iter (add1 cnt) mat (matrix-index-of-elt mat water))
          (let* ([current-water-pos (car pending-water-pos-lst)]
                 [rest-pending-water-pos-lst (cdr pending-water-pos-lst)]
                 [new-pending-water-pos-lst (neighbor-water-pos-list mat current-water-pos)])
            (reduce-lake (replace-by-pos-list mat new-pending-water-pos-lst)
                         (append rest-pending-water-pos-lst new-pending-water-pos-lst))))))))


(lake-counting (list '(W - - - - - - - - W W -)
                     '(- W W W - - - - - W W W)
                     '(- - - - W W - - - W W -)
                     '(- - - - - - - - - W W -)
                     '(- - - - - - - - - W - -)
                     '(- - W - - - - - - W - -)
                     '(- W - W - - - - - W W -)
                     '(W - W - W - - - - - W -)
                     '(- W - W - - - - - - W -)
                     '(- - W - - - - - - - W -)))
