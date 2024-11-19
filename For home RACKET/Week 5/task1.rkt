#lang racket

(define (remove-all-no-proc element xs)
  (define (helper remaining-xs result-xs)
    (cond
      [(empty? remaining-xs) result-xs]
      [(not (equal? (car remaining-xs) element)) (helper (cdr remaining-xs)(cons (car remaining-xs) result-xs))]
      [else (helper (cdr remaining-xs) result-xs)]
      )
    )
  (reverse (helper xs '()))
  )

(define (remove-all-proc element xs)
  (remove* (list element) xs)
)

; without using a predefined procedure
(equal? (remove-all-no-proc 1 '(1 1 1 2)) '(2))
(equal? (remove-all-no-proc 1 '(2 5 6)) '(2 5 6))
(equal? (remove-all-no-proc 1 '(1)) '())
(equal? (remove-all-no-proc 1 '(1 2 1 1)) '(2))
(equal? (remove-all-no-proc "RNN" '("CNN" "RNN" "GAN" "RNN")) '("CNN" "GAN"))

; using a predefined procedure
(equal? (remove-all-proc 1 '(1 1 1 2)) '(2))
(equal? (remove-all-proc 1 '(2 5 6)) '(2 5 6))
(equal? (remove-all-proc 1 '(1)) '())
(equal? (remove-all-proc 1 '(1 2 1 1)) '(2))
(equal? (remove-all-proc "RNN" '("CNN" "RNN" "GAN" "RNN")) '("CNN" "GAN"))
