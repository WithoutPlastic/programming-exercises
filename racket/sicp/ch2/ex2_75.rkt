#lang racket

(define [make-from-real-imag real image]
  (lambda [op]
    (cond ([eq? op 'real-part] real)
          ([eq? op 'imag-part] image)
          ([eq? op 'magnitude]
           (sqrt (+ (square real) (square image))))
          ([eq? op 'angle]
           (atan image real))
          (else
            (error "Unknown op -- MAKE-FROM-REAL-IMAG" op)))))

(define [make-from-mag-ang mag ang]
  (lambda [op]
    (cond ([eq? op 'real-part] (cos mag))
          ([eq? op 'imag-part] (sin mag))
          ([eq? op 'magnitude] mag)
          ([eq? op 'angle] ang)
          (else
            (error "Unknown op -- MAKE-FROM-MAG-ANG" op)))))
