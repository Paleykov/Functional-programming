#lang racket
(require math/number-theory)

;Define a procedure that returns the number of occurrences of a digit in a non-negative number.

(define (count-occurrences number digit)
  (cond
    [(< number 0) (error "negative number")]
    [(and (< number 10) (= number digit) 1)]
    [(and (< number 10) (not (= number digit)) 0)]
    [(= (remainder number 10) digit) (add1 (count-occurrences (quotient number 10) digit))]
    [else (count-occurrences (quotient number 10) digit)]
    )
  )

(= (count-occurrences 121 1) 2)
(= (count-occurrences 222 1) 0)
(= (count-occurrences 100 0) 2)
(= (count-occurrences 0 0) 1)