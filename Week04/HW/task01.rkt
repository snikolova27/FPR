#lang racket


(define (upper-bound f y)
 (lambda (x)
 (if (>= y (f x))
       y
       (f x))
   )
 )


((upper-bound (lambda (x) (* 2 x)) 100) 50)
((upper-bound (lambda (x) (* 2 x)) 100.236) 500.002)
((upper-bound identity 1.001) 1.001)
((upper-bound (lambda (x) (* 2 x )) 80) 3) 