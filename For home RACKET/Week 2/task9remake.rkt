#lang racket
(require math/number-theory)

;Define a procedure sum-special-primes n d that returns the sum of the first n prime numbers that contain the digit d.

(define (contains-n? num n)
    (cond
      [(and (< num 10) (= num n)) #t]
      [(and (< num 10) (not (= num n))) #f]
      [(= (remainder num 10) n) #t]
      [else (contains-n? (quotient num 10) n)]
      )
    )
; the function "contains-n" is now global since it was not a helper for "sum-special-primes" but was only used by it

(define (sum-special-primes d n)
  (define (helper curr-num sum counter)
    (cond
      [(zero? counter) sum]
      [(and (prime? curr-num) (contains-n? curr-num n)) (helper (add1 curr-num) (+ sum curr-num) (sub1 counter))]
      [else (helper (add1 curr-num) sum counter)]
      ) 
    )
  (helper 1 0 d)
  )

(= (sum-special-primes 5 2) 392)
(= (sum-special-primes 5 3) 107)
(= (sum-special-primes 10 3) 462)