#lang racket

(define [attach-tag tag proc]
  (lambda args (cons tag (apply proc args))))
(define [extract-tag x] (car x))
(define [extract-data x] (cdr x))
(define op-table (make-hash))
(define [get-op op args-type-tags]
  (hash-ref op-table
            (list op args-type-tags)))
(define [put-op op args-type-tags return-type-tag proc ]
  (hash-set! op-table
             (list op args-type-tags)
             (attach-tag return-type-tag proc)))

(define [apply-generic op . args]
  (let* ([args-type-tags (map extract-tag args)]
         [proc (get-op op args-type-tags)])
    (if proc
      (apply proc (map extract-data args))
      (error "No method for these types -- APPLY-GENERIC"
             (list op args-type-tags)))))

(define [install-packages]
  (define [install-scheme-number-package]
    (put-op 'equ?
            '(scheme-number scheme-number)
            'bool
            (lambda [value-a value-b]
              (= value-a value-b)))
    (put-op 'zero?
            '(scheme-number)
            'bool
            (lambda [value] (= value 0)))
    (put-op 'make
            '(scheme-number)
            'scheme-number
            (lambda [value] value))
    (put-op 'add
            '(scheme-number scheme-number)
            'scheme-number
            (lambda [value-a value-b] (+ value-a value-b)))
    (put-op 'sub
            '(scheme-number scheme-number)
            'scheme-number
            (lambda [value-a value-b] (- value-a value-b)))
    (put-op 'multiple
            '(scheme-number scheme-number)
            'scheme-number
            (lambda [value-a value-b] (* value-a value-b)))
    (put-op 'divide
            '(scheme-number scheme-number)
            'scheme-number
            (lambda [value-a value-b] (/ value-a value-b))))

  (define [install-rational-package]
    (define [numer rational] (car rational))
    (define [denom rational] (car rational))
    (define [make-rational numer denom]
      (if [= (gcd numer denom) 1]
        (cons numer denom)
        (make-rational (/ numer (gcd numer denom)) (/ denom (gcd numer denom)))))

    (put-op 'equ?
            '(rational rational)
            'bool
            (lambda [value-a value-b]
              (and (= (numer value-a) (numer value-b))
                   (= (denom value-a) (denom value-b)))))
    (put-op 'zero?
            '(rational)
            'bool
            (lambda [value] (= (numer value) 0))
            )
    (put-op 'numer
            '(rational)
            'scheme-number
            (lambda [rational] (numer rational)))
    (put-op 'denom
            '(rational)
            'scheme-number
            (lambda [rational] (denom rational)))
    (put-op 'make
            '(scheme-number scheme-number)
            'rational
            (lambda [numer denom] (make-rational numer denom)))
    (put-op 'add
            '(rational rational)
            'rational
            (lambda [rational-a rational-b]
              (make-rational
                (+ (* (numer rational-a) (denom rational-b))
                   (* (numer rational-b) (denom rational-a)))
                (* (denom rational-a) (denom rational-b)))))
    (put-op 'sub
            '(rational rational)
            'rational
            (lambda [rational-a rational-b]
              (make-rational
                (- (* (numer rational-a) (denom rational-b))
                   (* (numer rational-b) (denom rational-a)))
                (* (denom rational-a) (denom rational-b)))))
    (put-op 'mul
            '(rational rational)
            'rational
            (lambda [rational-a rational-b]
              (make-rational
                (* (numer rational-a) (numer rational-b))
                (* (denom rational-a) (denom rational-b)))))
    (put-op 'div
            '(rational rational)
            'rational
            (lambda [rational-a rational-b]
              (make-rational
                (* (numer rational-a) (denom rational-b))
                (* (denom rational-a) (numer rational-b)))))
    )
  (define [install-complex-package]
    ;(define [install-complex-real-imag-package])
    ;(define [install-complex-mgnt-angl-package])
    (define [make-from-real-imag real imag] (cons real imag))
    (define [make-from-mag-ang mag ang]
      (cons (* mag (cos ang)) (* mag (sin ang))))
    (define [real-part complex] (car complex))
    (define [imag-part complex] (cdr complex))
    (define [magnitude complex]
      (sqrt (+ (expt (real-part complex) 2)
               (expt (imag-part complex) 2))))
    (define [angle complex]
      (atan (/ (imag-part complex) (real-part complex))))

    (define [add complex-a complex-b]
      (make-from-real-imag (+ (real-part complex-a) (real-part complex-b))
                           (+ (imag-part complex-a) (imag-part complex-b))))
    (define [sub complex-a complex-b]
      (make-from-real-imag (- (real-part complex-a) (real-part complex-b))
                           (- (imag-part complex-a) (imag-part complex-b))))
    (define [mul complex-a complex-b]
      (make-from-mag-ang (* (magnitude complex-a) (magnitude complex-b))
                         (+ (angle complex-a) (angle complex-b))))
    (define [div complex-a complex-b]
      (make-from-mag-ang (/ (magnitude complex-a) (magnitude complex-b))
                         (- (angle complex-a) (angle complex-b))))

    (put-op 'equ?
            '(complex complex)
            'complex
            (lambda [value-a value-b]
              (and (= (real-part value-a) (real-part value-b))
                   (= (imag-part value-a) (imag-part value-b)))))
    (put-op 'zero?
            '(complex)
            'bool
            (lambda [value] (and (= (real-part value) 0)
                                 (= (imag-part value) 0))))
    (put-op 'real-part
            '(complex)
            'scheme-number
            (lambda [complex] (real-part complex)))
    (put-op 'imag-part
            '(complex)
            'scheme-number
            (lambda [complex] (imag-part complex)))
    (put-op 'magnitude
            '(complex)
            'scheme-number
            (lambda [complex] (magnitude complex)))
    (put-op 'angle
            '(complex)
            'scheme-number
            (lambda [complex] (angle complex)))
    (put-op 'make ;Default make
            '(scheme-number scheme-number)
            'complex
            (lambda [real imag] (make-from-real-imag real imag)))
    (put-op 'make-from-real-imag
            '(scheme-number scheme-number)
            'complex
            (lambda [real imag] (make-from-real-imag real imag)))
    (put-op 'make-from-mag-ang
            '(scheme-number scheme-number)
            'complex
            (lambda [mag ang] (make-from-mag-ang mag ang)))
    (put-op 'add
            '(complex complex)
            'complex
            (lambda [complex-a complex-b] (add complex-a complex-b)))
    (put-op 'sub
            '(complex complex)
            'complex
            (lambda [complex-a complex-b] (sub complex-a complex-b)))
    (put-op 'mul
            '(complex complex)
            'complex
            (lambda [complex-a complex-b] (mul complex-a complex-b)))
    (put-op 'div
            '(complex complex)
            'complex
            (lambda [complex-a complex-b] (div complex-a complex-b))))

  (install-scheme-number-package)
  (install-rational-package)
  (install-complex-package))

(define [zero? x] (apply-generic 'zero? x))
(define [equ? x y] (apply-generic 'equ? x y))
(define [add x y] (apply-generic 'add x y))
(define [sub x y] (apply-generic 'sub x y))
(define [mul x y] (apply-generic 'mul x y))
(define [div x y] (apply-generic 'div x y))
(define [numer x] (apply-generic 'numer x))
(define [denom x] (apply-generic 'denom x))
(define [real-part x] (apply-generic 'real-part x))
(define [imag-part x] (apply-generic 'imag-part x))
(define [magnitude x] (apply-generic 'magnitude x))
(define [angle x] (apply-generic 'angle x))

(define [make-scheme-number x]
  ((get-op 'make '(scheme-number)) x)) ; No type dispatch
(define [make-rational x y]
  (apply-generic 'make x y))
(define [make-complex-from-real-imag real imag]
  (apply-generic 'make-from-real-imag real imag))
(define [make-complex-from-mag-ang mag ang]
  (apply-generic 'make-from-mag-ang mag ang))

(install-packages)
(add (make-complex-from-real-imag (make-scheme-number 1) (make-scheme-number 2))
     (make-complex-from-real-imag (make-scheme-number 4) (make-scheme-number 5)))
