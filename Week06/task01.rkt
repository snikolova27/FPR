#lang racket
(define (my-reverse-iter xs)
  (define (helper left-over res)
    (cond
      [(null? left-over) res]
      [else (helper (cdr left-over) (cons (car left-over) res))]
    )
  )
  (helper xs '())
)

(equal? (my-reverse-iter '(1 2 3 4 5)) '(5 4 3 2 1))