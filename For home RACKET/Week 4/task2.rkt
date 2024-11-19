#lang racket

(define (apply-n f n)
  (define (helper f counter)
    (if (= counter 1)
        f
        (λ (x) (f ((helper f (sub1 counter)) x)))
        )
    )
  (helper f n)
  )

(= ((apply-n (λ (x) (* 2 x)) 5) 2) 64)
(= ((apply-n (λ (x) (quotient x 10)) 2) 100) 1)