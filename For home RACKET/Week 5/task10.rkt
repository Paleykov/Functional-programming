#lang racket

(define (concat-proc xs1 xs2)
  (append xs1 xs2)
  )

(define (concat-rec xs1 xs2)
  (define (helper curr-list remainder)
    (if(empty? remainder)
       curr-list
       (helper (reverse (cons (car remainder) (reverse curr-list))) (cdr remainder))
       )
    )
  (helper xs1 xs2)
  )

; using a predefined procedure
(equal? (concat-proc '(1 2 3) '(4 5 6)) '(1 2 3 4 5 6))

; using a linearly iterative process
(equal? (concat-rec '(1 2 3) '(4 5 6)) '(1 2 3 4 5 6))