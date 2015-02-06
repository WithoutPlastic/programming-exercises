#lang racket

(interpret '(define adverbs '(adverb noisily quickly loudly)))
(interpret '(define (parse-verb-phrase)
              (define (maybe-extend verb-phrase)
                (amb verb-phrase
                     (maybe-extend (list 'verb-phrase
                                         verb-phrase
                                         (parse-prepositional-phrase)))))
              (maybe-extend (parse-simple-verb-phrase))))

(interpret '(define (parse-simple-verb-phrase)
              (amb (list 'simple-verb-phrase
                         (parse-word verbs))
                   (list 'adverb-phrase
                         (parse-word verbs)
                         (parse-word adverbs)))))

(define adjectives '(adjective nice tall diligent small white))
(define [parse-simple-noun-phrase]
  (define [parse-adjective-noun]
    (amb (parse-word nouns)
         (list 'adjective-noun
               (parse-word adjectives)
               (parse-adjective-noun))))
  (list 'simple-noun-phrase
        (parse-word articles)
        (parse-adjective-noun)))
