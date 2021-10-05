#lang racket

(define (fib-rec x)
  (cond
    [(< x 0) error "x cannot be negative"] 
    [(zero? x) 0]
    [(= x 1) 1]
    [else (+ (fib-rec(sub1 x))(fib-rec(- x 2)))]
  )
)
(= (fib-rec 11) 89)

(define (fib-iter x)
  (define (helper prev2 prev1 left-over)
   (cond
     [(zero? left-over) prev2]
     [(= left-over 1) prev1]
     [else (helper prev1 (+ prev1 prev2) (sub1 left-over))]
     )
  )
  (helper 0 1 x)
)

(= (fib-iter 11) 89)