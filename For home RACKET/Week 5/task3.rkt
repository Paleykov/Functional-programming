#lang racket

(define (rev-fold xs)
  (foldr (λ (x acc) (+ x (* acc 10))) 0 xs)
  )

(define (rev-lin-iter xs)
  (define (helper curr-list result)
    (if (empty? curr-list)
        result
        (helper (cdr curr-list) (+ (* result 10) (car curr-list)))
        )
    )
  (helper (reverse xs) 0)
  )

; using folding
(= (rev-fold '(1 2 3)) 321)
(= (rev-fold '(1 2 3 4 5 6 7 8 9)) 987654321)

; using a linearly iterative procedure
(= (rev-lin-iter '(1 2 3)) 321)
(= (rev-lin-iter '(1 2 3 4 5 6 7 8 9)) 987654321)