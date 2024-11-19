#lang racket

(define (derive f eps)
  (λ (x) (/ (- (f (+ x eps)) (f x)) eps))
  )

(define (derive-n f n eps)
  (define (helper f counter)
    (if (zero? counter)
        f
        (helper (derive f eps) (sub1 counter)))
    )
  (helper f n)
  )

(= ((derive-n (λ (x) (* 2 x x x)) 3 1e-3) 2) 12.000015203739167)