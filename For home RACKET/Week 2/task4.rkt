#lang racket
(require math/number-theory)

(define (is-even-if num)
  (if (even? num)
      "Yes"
      "No"
      )
  )

(define (is-even-guards num)
  (cond
    [(even? num) "Yes"]
    [else "No"]
    )
  )

  
(equal? (is-even-if 2) "Yes")
(equal? (is-even-if 15452) "Yes")
(equal? (is-even-if 321) "No")

(equal? (is-even-guards 2) "Yes")
(equal? (is-even-guards 15452) "Yes")
(equal? (is-even-guards 321) "No")