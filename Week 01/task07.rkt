#lang racket

(define (is-even-if x)
  (if (zero?(remainder x 2))
      "Yes"
      "No")
)

(equal? (is-even-if 2) "Yes")
(equal? (is-even-if 15452) "Yes")
(equal? (is-even-if 321) "No")

(define (is-even-guards x)
  (cond
    [(= (remainder x 2) 0 ) "Yes"]
    [else "No"]
  )
)
(equal? (is-even-guards 2) "Yes")
(equal? (is-even-guards 15452) "Yes")
(equal? (is-even-guards 321) "No")