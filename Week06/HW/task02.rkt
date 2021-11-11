#lang racket
(define (my-reverse-foldr xs)
  (foldr (lambda (x y) (append y (list x))) '() xs)
)
;(my-reverse-foldr '(1 2 3 4 5))
(equal? (my-reverse-foldr '(1 2 3 4 5)) '(5 4 3 2 1))