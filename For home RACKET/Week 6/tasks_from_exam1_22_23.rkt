#lang racket
(require math/number-theory)

;---------------------------------- TASK 1 ----------------------------------
(define (primes-prod x)
  (define (helper curr-num result)
    (cond
      [(> curr-num (sqrt x)) result]
      [(prime? curr-num) (helper (add1 curr-num) (* result curr-num))]
      [else (helper (add1 curr-num) result)]
      )  
    )
  (helper 2 1)
  )

(= (primes-prod 12) 6)
(= (primes-prod 49) 210)
(= (primes-prod 1200) 200560490130)

;---------------------------------- TASK 2 ----------------------------------

(define (shuffle-merge xs ys)
  (define (helper curr-xs curr-ys result)
    (cond
      [(null? curr-ys) (append result curr-xs)]
      [(null? curr-xs) (append result curr-ys)]
      [else (helper (cdr curr-xs) (cdr curr-ys) (reverse (cons (car curr-ys) (cons (car curr-xs) (reverse result)))))]
      )
    )
  (helper xs ys `())
  )

(equal? (shuffle-merge `(1) `()) '(1))
(equal?(shuffle-merge `(3 4 5) `(2)) '(3 2 4 5))
(equal?(shuffle-merge `(3 4 5) `(9 2)) '(3 9 4 2 5))
(equal?(shuffle-merge `(3 2 8) `(5 6 1 9 11)) '(3 5 2 6 8 1 9 11))

;---------------------------------- TASK 3 ----------------------------------
(define (g-l-sum limit)
  (define (helper num1 num2)
    (cond
      [(= (+ (gcd num1 num2) (lcm num1 num2)) limit) (cons num1 num2)]
      [else (helper (add1 num1) num2)]
      )
    )
  (helper 1 1)
  )

(g-l-sum 2)
(g-l-sum 14)
(g-l-sum 25)
(g-l-sum 1387)
 
