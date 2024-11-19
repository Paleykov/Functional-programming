#lang racket

;Define a procedure that returns a list of pairs comprising the cartesian product of two sets.
;Note: There is a built-in procedure (cartesian-product xs ys [zs ...]). This exercise is the only time you should not use it.

(define (cartesian-elem-and-list elem xs)
  (define (helper remainder-xs result-xs)
    (if (null? remainder-xs)
        result-xs
        (helper (cdr remainder-xs) (cons (cons elem (car remainder-xs)) result-xs))
        )
    )
  (reverse(helper xs `()))
  )


(define (my-cartesian-product set1 set2)
  (define (helper curr-set1 result-xs)
    (if(null? curr-set1)
       result-xs
       (helper (cdr curr-set1) (append result-xs (cartesian-elem-and-list (car curr-set1) set2)))
     )
    )
  (helper set1 `())
  )

(equal? (my-cartesian-product '(1 2) '(3 4)) '((1 . 3) (1 . 4) (2 . 3) (2 . 4)))
(equal? (my-cartesian-product '(1 2 3 4 5) '(6 7 8)) '((1 . 6) (1 . 7) (1 . 8) (2 . 6) (2 . 7) (2 . 8) (3 . 6) (3 . 7) (3 . 8) (4 . 6) (4 . 7) (4 . 8) (5 . 6) (5 . 7) (5 . 8)))