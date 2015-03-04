#lang racket

;Problem:
;Implement regular expression matching with support for '.' and '*'.
;
;'.' Matches any single character.
;'*' Matches zero or more of the preceding element.
;
;The matching should cover the entire input string (not partial).
;
;The function prototype should be:
;bool isMatch(const char *s, const char *p)
;
;Some examples:
;isMatch("aa", "a") → false
;isMatch("aa", "aa") → true
;isMatch("aaa", "aa") → false
;isMatch("aa", "a*") → true
;isMatch("aa", ".*") → true
;isMatch("ab", ".*") → true
;isMatch("aab", "c*a*b") → true

(define [regex->chars regex]
  (define [iter remaining-chars]
    (define [continue]
      (let ([first-char (car remaining-chars)]
            [rest-chars (cdr remaining-chars)])
        (cond ([eq? first-char #\*]
               (error "illegal regex -- REGEX->CHARS" regex))
              ([and [not [null? rest-chars]]
                    [eq? (car rest-chars) #\*]]
               (cons (cons '* first-char) (iter (cdr rest-chars))))
              (else (cons first-char (iter rest-chars))))))
    (if [null? remaining-chars] '() (continue)))
  
  (let ([chars (string->list regex)])
    (iter chars)))

(define [matched? str regex]
  (define [iter remaining-chars remaining-regex]
    (define [continue]
      (let ([first-char (car remaining-chars)]
            [first-regex-char (car remaining-regex)]
            [rest-chars (cdr remaining-chars)]
            [rest-regex-chars (cdr remaining-regex)])
        (define [asterisk-matching remainings char]
          (define [continue]
            (let ([frt (car remainings)]
                  [rst (cdr remainings)])
              (if [or [eq? char #\.] [eq? first-char char]]
                (or (asterisk-matching rst char)
                    (iter rst rest-regex-chars))
                (iter remainings rest-regex-chars))))

          (if [null? remainings]
            (iter remainings rest-regex-chars)
            (continue)))

        (cond ([pair? first-regex-char]
               (or (asterisk-matching rest-chars (cdr first-regex-char))
                   (iter remaining-chars (cdr remaining-regex))))
              ([or [eq? first-regex-char #\.]
                   [eq? first-char first-regex-char]]
               (iter rest-chars rest-regex-chars))
              (else false))))

    (cond ([and [null? remaining-chars] [null? remaining-regex]] true)
          ([or [null? remaining-chars] [null? remaining-regex]] false)
          (else (continue))))

  (let ([chars (string->list str)]
        [regex-chars (regex->chars regex)])
    (iter chars regex-chars)))

(matched? "aa" "a")
(matched? "aa" "aa")
(matched? "aaa" "aa")
(matched? "aa" "a*")
(matched? "aa" ".*")
(matched? "ab" ".*")
(matched? "aab" "c*a*b")
(matched? "abdffffftg" "abdf*g*tg")
