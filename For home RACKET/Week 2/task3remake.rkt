#lang racket

(define (are-not-equal-one-line? num1 num2)
  (not (= num1 num2))
  )

(define (are-not-equal-guards? num1 num2)
  (cond
    [(not (= num1 num2)) #t]
    [else #f]
    )
  )

(define (inside-one-line? num1 num2 between)
  (< (min num1 num2) between (max num1 num2))
  )

(define (inside-boolean-ops? num1 num2 between)
  (and
   (< (min num1 num2) between)
   (< between (max num1 num2))
   )
  )

; Now it makes sense

(equal? (are-not-equal-one-line? 5 2) #t)
(equal? (are-not-equal-one-line? 5 5) #f)

(equal? (are-not-equal-guards? 5 2) #t)
(equal? (are-not-equal-guards? 5 5) #f)

(equal? (inside-one-line? 1 5 4) #t) ; start = 1, end = 5, x = 4
(equal? (inside-one-line? 5 1 4) #t)
(equal? (inside-one-line? 10 50 200) #f)
(equal? (inside-one-line? 10 50 1) #f)

(equal? (inside-boolean-ops? 1 5 4) #t)
(equal? (inside-boolean-ops? 5 1 4) #t)
(equal? (inside-boolean-ops? 10 50 200) #f)
(equal? (inside-boolean-ops? 10 50 1) #f)
