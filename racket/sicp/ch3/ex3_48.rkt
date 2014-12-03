#lang racket

;make account provider id selector, and id can be comparable
(define [serialized-exchange account-a account-b]
  (let ([serializer-a (account-a 'serializer)]
        [serializer-b (account-b 'serializer)])
    (if [< (account-a 'id) (account-b 'id)]
      ((serializer-b (serializer-a exchange)) account-a account-b)
      ((serializer-a (serializer-b exchange)) account-a account-b))))

;when two procedure not cross acquire resources, then dead lock will never
;happen
