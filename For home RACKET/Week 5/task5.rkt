#lang racket
(require math/number-theory)

(define (factorize number)
  (define (helper curr-num remainder xs)
    (cond
      [(> curr-num remainder) xs]
      [(and (prime? curr-num) (divides? curr-num remainder)) (helper curr-num (/ remainder curr-num) (cons curr-num xs))]
      [else (helper (add1 curr-num) remainder xs)]
      )
    )
  (sort (helper 2 number `()) <)
  )

(equal? (factorize 2) '(2))
(equal? (factorize 6) '(2 3))
(equal? (factorize 13) '(13))
(equal? (factorize 123) '(3 41))
(equal? (factorize 152) '(2 2 2 19))
(equal? (factorize 12356498) '(2 7 11 19 41 103))