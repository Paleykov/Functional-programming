#lang racket

(define (count-digits-iter number)
  (define (helper num count)
    (if(< num 10)
      count
      (helper (quotient num 10) (add1 count))
      )
    ;swapped the cond with If
    )
  (if (< number 0)
      (error "number is negative")
      (helper number 1)
      )
  )

(define (count-digits-rec number)
  (cond
    [(< number 0) (error "negative number")]
    [(< number 10) 1]
    [else (add1 (count-digits-rec (quotient number 10)))]
    )
  )

(= (count-digits-iter 12345) 5)
(= (count-digits-iter 123) 3)

(= (count-digits-rec 12345) 5)
(= (count-digits-rec 123) 3)
;(count-digits-iter -13) ; error "n was negative"