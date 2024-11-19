#lang racket

;Define a procedure longest-ascending-sub xs that returns the longest sublist (sequence of consecutive elements) that is sorted in ascending order from xs.

(define (sublist-ascending xs)
  (define (helper remainder sub)
    (cond
      [(null? remainder) sub]
      [(>= (first remainder) (first sub)) (helper (cdr remainder) (cons (car remainder) sub))]
      [else sub]
      )
    )
  (reverse (helper (cdr xs) (list (car xs))))
  )

(define (longest-ascending-sub xs)
  (define (helper remainder-xs curr-longest-ascending-sub)
    (cond
      [(null? remainder-xs) curr-longest-ascending-sub]
      [(< (length curr-longest-ascending-sub) (length (sublist-ascending remainder-xs))) (helper (drop remainder-xs (length (sublist-ascending remainder-xs))) (sublist-ascending remainder-xs))]
      [else (helper (drop remainder-xs (length (sublist-ascending remainder-xs))) curr-longest-ascending-sub)]
      )
    )
  (helper xs `())
  )

(equal? (longest-ascending-sub '(1 0 5)) '(0 5))
(equal? (longest-ascending-sub '(1 5 2 3 1 5 6 7 7 1 5)) '(1 5 6 7 7))
(equal? (longest-ascending-sub '(1 5 2 3 1 5 2 7 7 15)) '(2 7 7 15))
(equal? (longest-ascending-sub '(1 5 2 3 4 5 6 7 7 1 5)) '(2 3 4 5 6 7 7))
(equal? (longest-ascending-sub '(1 5 2 4 6 8 3 4 1)) '(2 4 6 8))
