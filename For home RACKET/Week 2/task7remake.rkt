#lang racket
(require math/number-theory)

;Define a recursive and an iterative procedure that returns the number of palindromes in the interval [a, b].

(define (is-palindrome? number)
  (define (helper curr-num res-number)
    (cond
      [(< curr-num 10) (= number (+ curr-num (* res-number 10)))]
      [else (helper (quotient curr-num 10) (+ (* res-number 10) (remainder curr-num 10)))]
      )
    )
   (helper number 0)
  )

(define (num-palindromes-iter first last) 
  (define (helper curr-num count)
    (cond
      [(> curr-num (max first last)) count]
      [(is-palindrome? curr-num) (helper (add1 curr-num) (add1 count))]
      [else (helper (add1 curr-num) count)]
      )
    )
  (helper (min first last) 0)
  )

(define (num-palindromes-rec first last)
  (cond
    [(and (> (add1 (min first last)) (max first last)) (is-palindrome? (min first last))) 1]
    [(> (add1 (min first last)) (max first last)) 0]
    [(is-palindrome? (min first last)) (add1 (num-palindromes-rec (add1 (min first last)) (max first last)))]
    [else (num-palindromes-rec (add1 (min first last)) (max first last))]
  )
)

(= (num-palindromes-rec 1 101) 19)
(= (num-palindromes-rec 1 100) 18)
(= (num-palindromes-rec 100 1) 18)

(= (num-palindromes-iter 1 101) 19)
(= (num-palindromes-iter 1 100) 18)
(= (num-palindromes-iter 100 1) 18)
