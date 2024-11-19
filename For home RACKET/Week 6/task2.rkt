#lang racket

(define (check-ps? element p-xs)
  (cond
    [(null? p-xs) #t]
    [(equal? ((car p-xs) element) #t) (check-ps? element (cdr p-xs))]
    [else #f]
    )
  )

(define (where xs p-xs)
   (filter (lambda (x) (check-ps? x p-xs)) xs)
  )

(equal? (where '(3 4 5 6 7 8 9 10) (list even? (lambda (x) (> x 5)))) '(6 8 10)) ; all even numbers greater than 5
(equal? (where '(3 4 5 7) (list even? (lambda (x) (> x 5)))) '()) ; no numbers are even and greater than 5
(equal? (where '() (list odd? (λ (x) (> x 3)) (λ (x) (< x 20)))) '())
(equal? (where '(1 2 3 4 5 6 7 8 9 10 11 13 15) (list odd? (λ (x) (> x 3)) (λ (x) (< x 20)))) '(5 7 9 11 13 15))