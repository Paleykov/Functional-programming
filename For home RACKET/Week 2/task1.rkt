#lang racket

(define (sum-cubes-pow num1 num2)
  (+ (expt num1 3) (expt num2 3))
  )

(define (sum-cubes-no-pow num1 num2)
  (+ (* num1 num1 num1) (* num2 num2 num2))
  )

(= (sum-cubes-pow 5 1) 126)
(= (sum-cubes-pow 10 50) 126000)

(= (sum-cubes-no-pow 5 1) 126)
(= (sum-cubes-no-pow 10 50) 126000)