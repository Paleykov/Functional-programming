#lang racket

(define (sq-avg num1 num2)
  (/ (+ (expt num1 2) (expt num2 2)) 2)
  )

(= (sq-avg 5 0) 12.5)
(= (sq-avg 10 13) 134.5)