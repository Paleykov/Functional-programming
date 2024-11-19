#lang racket
(require racket/trace)

;Define a procedure that takes a list of numbers and returns a list of pairs in the form (xi . ni) where xi spans the elements of xs and ni is the number of elements in xs that are greater than xi.

(define (num-bigger-elements xs)
  (define (helper curr-el remainder-list pairs-list)
    (if (null? remainder-list)
        (cons (cons curr-el (length (filter (λ (x) (< curr-el x)) xs))) pairs-list)
        ;For some reason the last pair never makes it in the list with the helper so I have to do the check again when we reach the end of the list 
        (helper
         (car remainder-list)
         (cdr remainder-list)
         (cons (cons curr-el (length (filter (λ (x) (< curr-el x)) xs))) pairs-list)
         )
        )
    )
  (reverse (helper (car xs) (cdr xs) `()))
  )

(equal? (num-bigger-elements '(5 6 3 4)) '((5 . 1) (6 . 0) (3 . 3) (4 . 2)))
(equal? (num-bigger-elements '(1 1 1)) '((1 . 0) (1 . 0) (1 . 0)))