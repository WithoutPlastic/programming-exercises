#lang racket

;Problem:
;Given a string containing only digits, restore it by returning all possible
;valid IP address combinations.
;
;For example:
;Given "25525511135",
;
;return ["255.255.11.135", "255.255.111.35"]. (Order does not matter)

(define dot #\.)
(define [char->number c] (- (char->integer c) 48))
(define [chars->integer chars]
  (let* ([len (length chars)]
         [dec-weights (reverse (map (curry expt 10) (range 0 len)))])
    (apply + (map * (map char->number chars) dec-weights))))

(define IP-ADDR-MAX-SLICE 256)
(define IP-ADDR-SLICE-DROP-LAST (floor (/ IP-ADDR-MAX-SLICE 10)))
(define IP-ADDR-SLICE-LAST-BIT (remainder IP-ADDR-MAX-SLICE 10))

(define [restore-ip-addresses str]
  (define [iter remaining accum]
    (define [continue]
      (let* ([first-char (car remaining)]
             [rest-chars (cdr remaining)]
             [first-num (char->number first-char)]
             [cur-slice (last accum)]
             [slice-len (length cur-slice)]
             [slice-number (chars->integer cur-slice)]
             [exted-accum (append accum (list (list first-char)))]
             [exted-slice (list (append cur-slice (list first-char)))]
             [exted-slice-accum (append (drop-right accum 1) exted-slice)])
        (cond ([= slice-len 1] (append (iter rest-chars exted-slice-accum)
                                       (iter rest-chars exted-accum)))
              ([= slice-len 3] (iter rest-chars exted-accum))
              ([or [< IP-ADDR-SLICE-DROP-LAST slice-number]
                   [<= IP-ADDR-SLICE-LAST-BIT first-num]]
               (iter rest-chars exted-accum))
              (else (append (iter rest-chars exted-slice-accum)
                            (iter rest-chars exted-accum))))))

    (let ([accum-len (length accum)])
      (cond ([= accum-len 5] '())
            ([and [= accum-len 4] [null? remaining]]
             (list (list->string
                     (apply append (add-between accum (list dot))))))
            (else (continue)))))

  (let ([chars (string->list str)])
    (iter (cdr chars) (list (list (car chars))))))

(define test-ip-addr "25525511135")

(restore-ip-addresses test-ip-addr)
