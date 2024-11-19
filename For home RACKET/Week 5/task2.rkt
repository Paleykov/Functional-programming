#lang racket

(define (set-union xs1 xs2)
  (sort (remove-duplicates (append xs1 xs2)) <)
  )

(equal? (set-union '(1 3 5 7) '(5 7 13)) '(1 3 5 7 13))
(equal? (set-union '(5 7 13) '(1 3 5 7)) '(1 3 5 7 13))