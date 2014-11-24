#lang racket

(define [attach-tag tag proc]
  (lambda args (cons tag (apply proc args))))
(define [extract-tag x] (if [number? x] 'scheme-number (car x)))
(define [extract-data x] (if [number? x] x (cdr x)))
(define op-table (make-hash))
(define [has-op? op args-type-tags]
  (hash-has-key? op-table (list op args-type-tags)))
(define [get-op op args-type-tags]
  (hash-ref op-table (list op args-type-tags)))
(define [put-op op args-type-tags return-type-tag proc]
  (if [eq? return-type-tag 'bool]
    (hash-set! op-table
               (list op args-type-tags)
               proc)
    (hash-set! op-table
               (list op args-type-tags)
               (attach-tag return-type-tag proc))))

;convert op table share individual table
;two input type tags is different, indicate a convert procedure
(define convert-table (make-hash))
(define [has-convert? from-type to-type]
  (hash-has-key? (list from-type to-type)))
(define [get-convert from-type to-type]
  (hash-ref convert-table (list from-type to-type)))
(define [put-convert from-type to-type proc]
  (if [eq? to-type 'null]
    (hash-set! convert-table
               (list from-type to-type)
               (lambda [value] (proc (extract-data value))))
    (hash-set! convert-table
               (list from-type to-type)
               (attach-tag to-type proc))))

(define type-chain (list 'scheme-number 'rational 'complex))
(define [has-raise-convert-key? from-type]
  (< 1 (length (memq from-type type-chain))))
(define [get-raise-convert-key from-type]
  (list from-type (cadr (memq from-type type-chain))))
(define [can-be-raise? value] (has-raise-convert-key? (extract-tag value)))
(define [raise value]
  ((apply get-convert (get-raise-convert-key (extract-tag value))) value))

(define [has-drop-convert-key? from-type]
  (not (eq? from-type (car type-chain))))
(define [get-drop-convert-key from-type] (list from-type 'null))
(define [can-be-drop? value] (has-drop-convert-key? (extract-tag value)))
(define [drop value]
  ((apply get-convert (get-drop-convert-key (extract-tag value))) value))
(define [loseless-drop? value]
  (equ? value (raise (drop value))))
(define [deep-drop value]
  (if [and (can-be-drop? value) (loseless-drop? value)]
    (deep-drop (drop value))
    value))

(define [apply-generic op . args]
  (if [< (length args) 3]
    (apply apply-operator op args)
    (apply apply-generic
           op
           (apply-operator op (car args) (cadr args))
           (cddr args))))

(define [apply-operator op . args]
  (let* ([args-type-tags (map extract-tag args)])
    (define op-matched? (has-op? op args-type-tags))
    (define [launch-op]
      (apply (get-op op args-type-tags)
             (map extract-data args)))
    ;Only two input args
    (define args-type-same?
      (or (= (length args-type-tags) 1)
          (eq? (car args-type-tags) (cadr args-type-tags)))) 
    (define [convert-and-retry]
      (let* ([first-arg (car args)]
             [second-arg (cadr args)])
        (cond ([can-be-raise? first-arg]
               (apply-operator
                 op
                 (raise first-arg)
                 second-arg))
              ([can-be-raise? second-arg]
               (apply-operator
                 op
                 first-arg
                 (raise second-arg)))
              (else (error "No available type convert" args)))))

    (cond (op-matched? (launch-op))
          ([not args-type-same?]
           (convert-and-retry))
          (args-type-same?
           (error "No method for these types" (list op args))))))

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
    (put-op 'mul
            '(scheme-number scheme-number)
            'scheme-number
            (lambda [value-a value-b] (* value-a value-b)))
    (put-op 'divide
            '(scheme-number scheme-number)
            'scheme-number
            (lambda [value-a value-b] (/ value-a value-b)))
    (put-op 'gcd
            '(scheme-number scheme-number)
            'scheme-number
            (lambda [value-a value-b] (gcd value-a value-b)))
    (put-op 'cos
            '(scheme-number)
            'scheme-number
            (lambda [value] (cos value)))
    (put-op 'sin
            '(scheme-number)
            'scheme-number
            (lambda [value] (sin value)))
    (put-op 'atan
            '(scheme-number)
            'scheme-number
            (lambda [value] (atan value)))
    (put-op 'sqrt
            '(scheme-number)
            'scheme-number
            (lambda [value] (sqrt value)))
    (put-op 'expt
            '(scheme-number scheme-number)
            'scheme-number
            (lambda [value-a value-b] (expt value-a value-b))))

  (define [install-rational-package]
    (define [numer rational] (car rational))
    (define [denom rational] (cdr rational))
    (define [make-rational numer denom]
      (let ([gcd-of-numer-denom (alt-gcd numer denom)])
        (if [equ? gcd-of-numer-denom 1]
          (cons numer denom)
          (make-rational (div numer gcd-of-numer-denom)
                         (div denom gcd-of-numer-denom)))))

    (put-op 'equ?
            '(rational rational)
            'bool
            (lambda [value-a value-b]
              (and (equ? (numer value-a) (numer value-b))
                   (equ? (denom value-a) (denom value-b)))))
    (put-op 'zero?
            '(rational)
            'bool
            (lambda [value] (equ? (numer value) (make-scheme-number 0)))
            )
    (put-op 'numer
            '(rational)
            'scheme-number
            (lambda [rational] (numer rational)))
    (put-op 'denom
            '(rational)
            'scheme-number
            (lambda [rational] (denom rational)))
    (put-op 'make-rational
            '(scheme-number scheme-number)
            'rational
            (lambda [numer denom] (make-rational numer denom)))
    (put-op 'add
            '(rational rational)
            'rational
            (lambda [rational-a rational-b]
              (make-rational
                (add (mul (numer rational-a) (denom rational-b))
                     (mul (numer rational-b) (denom rational-a)))
                (mul (denom rational-a) (denom rational-b)))))
    (put-op 'sub
            '(rational rational)
            'rational
            (lambda [rational-a rational-b]
              (make-rational
                (sub (mul (numer rational-a) (denom rational-b))
                     (mul (numer rational-b) (denom rational-a)))
                (mul (denom rational-a) (denom rational-b)))))
    (put-op 'mul
            '(rational rational)
            'rational
            (lambda [rational-a rational-b]
              (make-rational
                (mul (numer rational-a) (numer rational-b))
                (mul (denom rational-a) (denom rational-b)))))
    (put-op 'div
            '(rational rational)
            'rational
            (lambda [rational-a rational-b]
              (make-rational
                (mul (numer rational-a) (denom rational-b))
                (mul (denom rational-a) (numer rational-b)))))

    (put-convert 'scheme-number
                 'rational
                 (lambda [value]
                   (make-rational value (make-scheme-number 1))))
    (put-convert 'rational
                 'null
                 (lambda [value] (numer value))))

  (define [install-complex-package]
    ;(define [install-complex-real-imag-package])
    ;(define [install-complex-mgnt-angl-package])
    (define [make-from-real-imag real imag]
      (cons real imag))
    (define [make-from-mag-ang mag ang]
      (make-from-real-imag (mul mag (cos ang))
                           (mul mag (sin ang))))
    (define [real-part complex] (car complex))
    (define [imag-part complex] (cdr complex))
    (define [magnitude complex]
      (sqrt (add (expt (real-part complex) (make-scheme-number 2))
                 (expt (imag-part complex) (make-scheme-number 2)))))
    (define [angle complex]
      (atan (div (imag-part complex) (real-part complex))))

    (put-op 'equ?
            '(complex complex)
            'bool
            (lambda [value-a value-b]
              (and (equ? (real-part value-a) (real-part value-b))
                   (equ? (imag-part value-a) (imag-part value-b)))))
    (put-op 'zero?
            '(complex)
            'bool
            (lambda [value]
              (and (equ? (real-part value)
                         (make-scheme-number 0))
                   (equ? (imag-part value)
                         (make-scheme-number 0)))))
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
            (lambda [complex-a complex-b]
              (make-from-real-imag
                (add (real-part complex-a) (real-part complex-b))
                (add (imag-part complex-a) (imag-part complex-b)))))
    (put-op 'sub
            '(complex complex)
            'complex
            (lambda [complex-a complex-b]
              (make-from-real-imag
                (sub (real-part complex-a) (real-part complex-b))
                (sub (imag-part complex-a) (imag-part complex-b)))))
    (put-op 'mul
            '(complex complex)
            'complex
            (lambda [complex-a complex-b]
              (make-from-mag-ang
                (mul (magnitude complex-a) (magnitude complex-b))
                (mul (angle complex-a) (angle complex-b)))))
    (put-op 'div
            '(complex complex)
            'complex
            (lambda [complex-a complex-b]
              (make-from-mag-ang
                (div (magnitude complex-a) (magnitude complex-b))
                (div (angle complex-a) (angle complex-b)))))

    (put-convert 'rational
                 'complex
                 (lambda [value]
                   (make-from-real-imag value
                                        (make-scheme-number 0))))
    (put-convert 'complex
                 'null
                 (lambda [value] (real-part value))))

  (install-scheme-number-package)
  (install-rational-package)
  (install-complex-package))

(define [zero? x] (apply-operator 'zero? x))
(define [equ? x y] (apply-operator 'equ? x y))
(define [add . args] (deep-drop (apply apply-generic 'add args)))
(define [sub . args] (deep-drop (apply apply-generic 'sub args)))
(define [mul . args] (deep-drop (apply apply-generic 'mul args)))
(define [div . args] (deep-drop (apply apply-generic 'div args)))
(define [alt-gcd . args] (apply apply-generic 'gcd args))
(define [numer x] (apply-operator 'numer x))
(define [denom x] (apply-operator 'denom x))
(define [sin x] (apply-operator 'sin x))
(define [cos x] (apply-operator 'cos x))
(define [atan x] (apply-operator 'atan x))
(define [sqrt x] (apply-operator 'sqrt x))
(define [expt x] (apply-operator 'expt x))
(define [real-part x] (apply-operator 'real-part x))
(define [imag-part x] (apply-operator 'imag-part x))
(define [magnitude x] (apply-operator 'magnitude x))
(define [angle x] (apply-operator 'angle x))

;input type racket number always no type dispatch
(define [make-scheme-number x]
  ((get-op 'make '(scheme-number)) x)) 
(define [make-rational x y]
  ((get-op 'make-rational '(scheme-number scheme-number)) x y))
(define [make-complex-from-real-imag real imag]
  ((get-op 'make-from-real-imag '(scheme-number scheme-number)) real imag))
(define [make-complex-from-mag-ang mag ang]
  ((get-op 'make-from-mag-ang '(scheme-number scheme-number)) mag ang))

(install-packages)
;(add (make-complex-from-real-imag (make-scheme-number 1)
;                                  (make-scheme-number 2))
;     (make-complex-from-real-imag (make-scheme-number 4)
;                                  (make-scheme-number 5)))
(loseless-drop? '(complex (scheme-number . 5) scheme-number . 2))
(add (make-complex-from-real-imag (make-scheme-number 4)
                                (make-scheme-number 5))
   (make-scheme-number 1))
(add (make-scheme-number 1)
     (make-scheme-number 1)
     (make-complex-from-real-imag (make-scheme-number 4)
                                  (make-scheme-number 5)))
