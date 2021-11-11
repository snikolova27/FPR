#lang racket

(define (my-reverse-foldl lst)
  (foldl (lambda (x acc) (cons x acc)) '() lst) 
)

(equal? (my-reverse-foldl '(1 2 3 4 5)) '(5 4 3 2 1))