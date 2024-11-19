#lang racket
(require math/number-theory)

;Define a predicate that accepts a natural number n and returns whether n^2 ends in the digits of n.

(define (count-digits n)
  (if (< n 10)
      1
      (add1 (count-digits (quotient n 10)))
      )
  )
;made the function global and replaced the cond with if since we only have 1 condition

(define (automorphic? n)
  (if (< n 1)
      (error "n was not natural")
      (= (remainder (expt n 2) (expt 10 (count-digits n))) n)
  )
)
;replaced the cond with if since we only have 1 condition and replaced the else with a bool equation

(equal? (automorphic? 3)#f)
(equal? (automorphic? 10)#f)
(equal? (automorphic? 5)#t)
(equal? (automorphic? 25)#t)
(equal? (automorphic? 76)#t) 
(equal? (automorphic? 890625)#t) 
(equal? (automorphic? 625)#t) 
(equal? (automorphic? 36) #f)
(equal? (automorphic? 11) #f)
;(automorphic? -1) ; error: n was not natural
;(automorphic? 0) ; error: n was not natural