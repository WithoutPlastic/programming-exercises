#lang racket

;Problem:
;Determine whether an integer is a palindrome. Do this without extra space.
;
;Some hints:
;Could negative integers be palindromes? (ie, -1)
;
;If you are thinking of converting the integer to string, note the restriction
;of using extra space.
;
;You could also try reversing an integer. However, if you have solved the
;problem "Reverse Integer", you know that the reversed integer might overflow.
;How would you handle such case?
;
;There is a more generic way of solving this problem.

(define [number-palindrome? num]
  (define [backward h-idx l-idx]
    (cond ([< l-idx 1] true)
          ([= (remainder (floor (/ num (expt 10 (sub1 h-idx)))) 10)
              (remainder (floor (/ num (expt 10 (sub1 l-idx)))) 10)]
           (backward (add1 h-idx) (sub1 l-idx)))
          (else false)))

  (define [iter cnt]
    (let ([h-idx (* 2 cnt)]
          [l-idx cnt])
      (cond ([< 9 cnt] 'overflow)
            ([= (floor (/ num (expt 10 (sub1 h-idx)))) 0]
             (backward (add1 l-idx) (sub1 l-idx)))
            ([= (floor (/ num (expt 10 h-idx))) 0]
             (backward (add1 l-idx) l-idx))
            (else (iter (add1 cnt))))))
  
  (cond ([< num 0] false)
        ([= (floor (/ num 10)) 0] true)
        (else (iter 1))))

(number-palindrome? 12300321)
(number-palindrome? 123050321)
(number-palindrome? 0)
(number-palindrome? -1024)
(number-palindrome? 2147483646)
(number-palindrome? 21474836462147483646)
