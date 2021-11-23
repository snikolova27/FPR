#lang racket

(define (where list-elements list-predicates)
  (define (helper result left-over)
    (cond
      [(null? left-over) result]
      [else (helper (filter (car left-over) result ) (cdr left-over))]
    )
  )
  (helper list-elements list-predicates)
)

(equal? (where '(3 4 5 6 7 8 9 10) (list even? (lambda (x) (> x 5)))) '(6 8 10) )
(equal? (where '(3 4 5 7) (list even? (lambda (x) (> x 5)))) '())