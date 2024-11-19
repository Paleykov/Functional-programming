#lang racket
(require math/number-theory)

#|
Given a divisor d and a bound b, find the largest integer N, such that:

N is divisible by d
and N is less than or equal to b
and N is greater than 0.
|#

(define (max-multiple divisor bound)
  (define (helper curr-num curr-max-div)
    (cond
      [(> curr-num bound) curr-max-div]
      [(divides? divisor curr-num) (helper (add1 curr-num) curr-num)]
      [else (helper (add1 curr-num) curr-max-div)]
      )
    )  
  (helper 1 1)
  )

(= (max-multiple 2 7) 6)
(= (max-multiple 3 10) 9)
(= (max-multiple 7 17) 14)
(= (max-multiple 10 50) 50)
(= (max-multiple 37 200) 185)
(= (max-multiple 7 100) 98)
