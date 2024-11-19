#lang racket

(define (insert-at x i xs)
  (define (helper left-side-xs right-side-xs curr-pos)
    (cond
      [(< (length xs) i) (error "Invalid index")]
      [(= curr-pos i) (append left-side-xs (cons x right-side-xs))]
      [else (helper (reverse (cons (car right-side-xs) (reverse left-side-xs))) (cdr right-side-xs) (add1 curr-pos))]
      )
    )
  (helper '() xs 0)
  )

(equal? (insert-at 1 0 '()) '(1))
(equal? (insert-at 1 0 '(2)) '(1 2))
(equal? (insert-at 10 1 '(1 2 3)) '(1 10 2 3))
(equal? (insert-at 1 0 '()) '(1))
(equal? (insert-at 1 0 '(2)) '(1 2))
(equal? (insert-at 10 1 '(1 2 3)) '(1 10 2 3))
(equal? (insert-at 7 0 '(1 2 3)) '(7 1 2 3))
(equal? (insert-at 7 1 '(1 2 3)) '(1 7 2 3))
(equal? (insert-at 7 3 '(1 2 3)) '(1 2 3 7))
(insert-at 7 4 '(1 2 3)) ; error: Invalid index!