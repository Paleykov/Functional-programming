#lang racket

(define (procedure f y)
  (λ (x) (if(>= y (f x))
            y
            (f x) 
            )
    )
  )

(define (f x) (* 2 x))
(= ((procedure f 100) 50) 100)
(= ((procedure f 100.236) 500.002) 1000.004)
(= ((procedure f 80) 3) 80)
(= ((procedure f 70) 30) 70)
(= ((procedure f 30) 70) 140)