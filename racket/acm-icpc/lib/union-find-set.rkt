#lang racket


(define type-label 'union-find-set )
(define end-guard 'union-find-set-end-guard)


(define [make-union-find-set val]
  (list type-label val (mcons end-guard '())))

(define [union-find-set? val]
  [and [list? val] [not [null? val]] [eq? (car val) type-label]])

(define union-find-set-label car)
(define union-find-set-value cadr)
(define union-find-set-next (compose mcar caddr))
(define [set-union-find-set-next! from-dfs to-dfs]
  (set-mcar! (caddr from-dfs) to-dfs))


(define [union-find-set-root ufs]
  (if [eq? (union-find-set-next ufs) end-guard] ufs
    (union-find-set-root (union-find-set-next ufs))))

(define union-find-set-root-value (compose union-find-set-value union-find-set-root))


(define [union-find-set-union! ufs-a ufs-b]
  (let ([root-a (union-find-set-root ufs-a)]
        [root-b (union-find-set-root ufs-b)])
    (set-union-find-set-next! root-a root-b)))


(define [union-find-set-same? val-a val-b]
  (let ([root-a (union-find-set-root val-a)]
        [root-b (union-find-set-root val-b)])
    [eq? root-a root-b]))


(define [union-find-set-root! ufs val]
  (let ([new-union-find-set (make-union-find-set val)]
        [ufs-root (union-find-set-root ufs)])
    (set-union-find-set-next! ufs-root new-union-find-set)))


(provide (rename-out [make-union-find-set uf-new]
                     [union-find-set? uf-set?]
                     [union-find-set-root-value uf-find]
                     [union-find-set-union! uf-union!]
                     [union-find-set-same? uf-same-set?]
                     [set-union-find-set-next! uf-set-canonical!]))
