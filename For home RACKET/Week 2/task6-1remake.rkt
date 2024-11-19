#lang racket
(require math/number-theory)

;Define a linearly iterative procedure for calculating the sum of the digits of a non-negative number.

(define (sum-digits-iter number)
  (define (helper num-curr sum)
    (if (< num-curr 10)
      (+ sum num-curr)
      (helper (quotient num-curr 10) (+ sum (remainder num-curr 10)))
      )
    ;swapped the cond for if
    )
  
  (if (negative? number) 
      ; Originally I did the task using negative? however I was not sure if I should since technically 0 is not a negative number and we are calculating the sum of digits on a non-negative number
      ; And negative? includes 0
    (error "negative number")
    (helper number 0)
    )
  )

(= (sum-digits-iter 12345) 15)
(= (sum-digits-iter 123) 6)
(sum-digits-iter -13) ; error "n was negative"