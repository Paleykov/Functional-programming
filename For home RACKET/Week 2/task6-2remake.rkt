#lang racket
(require math/number-theory)

; Define a procedure that returns the sum of all prime divisors of a given number.

(define (sum-prime-divs-rec number)
  (define (helper divisor)
    (cond
      [(> divisor number) 0]
      [(and (divides? divisor number) (prime? divisor)) (+ divisor (helper (add1 divisor)))]
      [else (helper (add1 divisor))]
      )
    )
  (helper 1)
  )

; fixed the "curr-num" name with a better one and redefined the procedure

(= (sum-prime-divs-rec 0) 0)
(= (sum-prime-divs-rec 6) 5) ; 2 + 3
(= (sum-prime-divs-rec 18) 5) ; 2 + 3
(= (sum-prime-divs-rec 19) 19)
(= (sum-prime-divs-rec 45136) 53)