#lang racket

;Problem:
;Given two binary strings, return their sum (also a binary string).
;
;For example,
;a = "11"
;b = "1"
;Return "100".

(define zero #\0) (define one #\1)
(define zero-eq? (curry eq? zero))
(define one-eq? (curry eq? one))

(define [add-binary bstr-a bstr-b]
  (let* ([bchars-a (string->list bstr-a)] [bchars-b (string->list bstr-b)]
         [len-a (length bchars-a)] [len-b (length bchars-b)]
         [l-bchars (if [< len-a len-b] bchars-b bchars-a)]
         [s-bchars (if [< len-a len-b] bchars-a bchars-b)]
         [short-len (min len-a len-b)] [long-len (max len-a len-b)])
    (define [iter offset c]
      (let ([cur-idx-s (- (sub1 short-len) offset)]
            [cur-idx-l (- (sub1 long-len) offset)])
        (define [continue-with-both]
          (let* ([cur-bchar-s (list-ref s-bchars cur-idx-s)]
                 [cur-bchar-l (list-ref l-bchars cur-idx-l)]
                 [ones-cnt (count one-eq? (list cur-bchar-s cur-bchar-l c))]
                 [next-offset (add1 offset)])
            (cond ([= ones-cnt 3] (append (iter next-offset one) (list one)))
                  ([= ones-cnt 2] (append (iter next-offset one) (list zero)))
                  ([= ones-cnt 1] (append (iter next-offset zero) (list one)))
                  (else (append (iter next-offset zero) (list zero))))))

        (define [continue-with-long]
          (let ([cur-bchar-l (list-ref l-bchars cur-idx-l)])
            (cond ([and [one-eq? c] [one-eq? cur-bchar-l]]
                   (append (iter (add1 offset) one) (list zero)))
                  ([or [one-eq? c] [one-eq? cur-bchar-l]]
                   (append (take l-bchars cur-idx-l) (list one)))
                  (else (append (take l-bchars cur-idx-l) (list zero))))))

        (cond ([< offset short-len] (continue-with-both))
              ([< offset long-len] (continue-with-long))
              ([one-eq? c] (list one))
              (else '()))))

    (iter 0 zero)))

(define test-bstr-a     "1010000100101")
(define test-bstr-b "11111100101111001")

(add-binary test-bstr-a test-bstr-b)
