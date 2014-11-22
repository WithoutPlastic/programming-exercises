#lang racket

(define [apply-generic op . args]
  (let* ([type-tags (map type-tag args)]
         [proc (get op type-tags)])
    (cond ([proc] (apply proc (map contents args)))
          ([= (length args) 2]
           (let* ([type-a (car type-tags)]
                  [type-b (cadr type-tags)]
                  [value-a (car args)]
                  [value-b (cadr args)]
                  [type-a->b (get-coercion type-a type-b)]
                  [type-b->a (get-coercion type-b type-a)])
             (cond (type-a->b (apply-generic op (type-a->b value-a) value-b))
                   (type-b->a (apply-generic op value-a (type-b->a value-b)))
                   (else
                     (error "No method for these types" (list op type-tags))))))
          (else (error "No method for these types" (list op type-tags))))))

(define [apply-generic op . args]
  (let* ([type-tags (map type-tag args)]
         [proc (get op type-tags)])
    (cond ([proc] (apply proc (map contents args)))
          ([and (= (length args) 2) (equal? (car type-tags) (cadr type-tags))]
           (error "No method for these types" (list op type-tags)))
          ([and (= (length args) 2) (not (equal? (car type-tags) (cadr type-tags)))]
           (let* ([type-a (car type-tags)]
                  [type-b (cadr type-tags)]
                  [value-a (car args)]
                  [value-b (cadr args)]
                  [type-a->b (get-coercion type-a type-b)]
                  [type-b->a (get-coercion type-b type-a)])
             (cond (type-a->b (apply-generic op (type-a->b value-a) value-b))
                   (type-b->a (apply-generic op value-a (type-b->a value-b)))
                   (else
                     (error "No method for these types" (list op type-tags))))))
          (else (error "No method for these types" (list op type-tags))))))

