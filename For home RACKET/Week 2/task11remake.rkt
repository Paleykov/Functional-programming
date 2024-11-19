#lang racket
(require math/number-theory)

;A number is interesting if and only if it is evenly divided by the sum of its digits. Define a predicate that checks whether a number is interesting.

(define (sum-of-digits num)
  (if(< num 10)
     num
     (+ (remainder num 10) (sum-of-digits (quotient num 10)))
     )
  )
;replaced the cond with if since we only have 1 condition and made the function global since it used by the interesting?, it is not a helper

(define (interesting? number)
  (divides? (sum-of-digits number) number)
  ;removed the if
)
(equal? (interesting? 410) #t)
(equal? (interesting? 212) #f)
(equal? (interesting? 567) #f)
(equal? (interesting? 70) #t)
(equal? (interesting? 5) #t)
(equal? (interesting? 4) #t)