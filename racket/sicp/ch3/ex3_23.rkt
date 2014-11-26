#lang racket

(define [make-deque]
  ;(<load> . (<to-tail-ptr> . <to-head-ptr))
  (define deque (mcons '() '()))
  (define [empty?] (or (null? (front-ptr)) (null? (rear-ptr))))
  (define [front-ptr] (mcar deque))
  (define [rear-ptr] (mcdr deque))
  (define [front] (mcar (mcar deque)))
  (define [rear] (mcar (mcdr deque)))
  (define [set-front-ptr! item] (set-mcar! deque item))
  (define [set-rear-ptr! item] (set-mcdr! deque item))
  (define [node-front-ptr node] (mcdr (mcdr node)))
  (define [node-rear-ptr node] (mcar (mcdr node)))
  (define [node-load node] (mcar node))
  (define [set-node-front-ptr! node item]
    (set-mcdr! (mcdr node) item))
  (define [set-node-rear-ptr! node item]
    (set-mcar! (mcdr node) item))
  (define [front-insert item]
    (let ([new-node (mcons item (mcons '() '()))])
      (if [empty?]
        (begin
          (set-front-ptr! new-node)
          (set-rear-ptr! new-node))
        (begin
          (set-node-rear-ptr! new-node (front-ptr))
          (set-node-front-ptr! (front-ptr) new-node)
          (set-front-ptr! new-node)))))
  (define [rear-insert item]
    (let ([new-node (mcons item (mcons '() '()))])
      (if [empty?]
        (begin
          (set-front-ptr! new-node)
          (set-rear-ptr! new-node))
        (begin
          (set-node-front-ptr! new-node (rear-ptr))
          (set-node-rear-ptr! (rear-ptr) new-node)
          (set-rear-ptr! new-node)))))
  (define [front-delete]
    (cond ([empty?] (error "front delete empty deque failed."))
          ([eq? (front-ptr) (rear-ptr)]
           (set! deque (mcons '() '())))
          (else
            (begin
              (set-node-front-ptr! (node-rear-ptr (front-ptr)) '())
              (set-front-ptr! (node-rear-ptr (front-ptr)))))))
  (define [rear-delete]
    (cond ([empty?] (error "rear delete empty deque failed."))
          ([eq? (front-ptr) (rear-ptr)]
           (set! deque (mcons '() '())))
          (else
            (begin
              (set-node-rear-ptr! (node-front-ptr (rear-ptr)) '())
              (set-rear-ptr! (node-front-ptr (rear-ptr)))))))
  (define [print]
    (define [iter node]
      (cond ([null? node] node)
            ([null? (node-rear-ptr node)] (node-load node))
            (else (cons (node-load node) (iter (node-rear-ptr node))))))
    (iter (front-ptr)))

  (lambda [op . args]
    (cond ([eq? op 'empty?] (empty?))
          ([eq? op 'front-insert] (apply front-insert args))
          ([eq? op 'rear-insert] (apply rear-insert args))
          ([eq? op 'front-delete] (front-delete))
          ([eq? op 'rear-delete] (rear-delete))
          ([eq? op 'front] (front))
          ([eq? op 'rear] (rear))
          ([eq? op 'print] (print))
          (else (error "undefined operation" op)))))

(define dq (make-deque))
(dq 'front-insert 'd)
(dq 'front-insert 'c)
(dq 'front-insert 'b)
(dq 'front-insert 'a)
(dq 'rear-insert 'e)
(dq 'print)

(dq 'rear-delete)
(dq 'rear-delete)
(dq 'rear-delete)
(dq 'front-delete)
(dq 'front-delete)
(dq 'print)
