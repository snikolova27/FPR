#lang racket

;counts nulls in factorial
(define (cnt-nulls n)
  (define (helper current-k res)
    (cond
      [(> (expt 5 current-k) n) res]
      [else (helper (add1 current-k) (+ res (quotient n (expt 5 current-k))))]
    )
  )
  (helper 1 0)
)

;(cnt-nulls 1000000000)

(define (trailing-zeros n)
  (lambda (p)
     (p (cnt-nulls n)))
)

((trailing-zeros 6) even?) ;->#f
((trailing-zeros 1000) even?) ;->#f
((trailing-zeros 100000) even?) ;->#f
((trailing-zeros 1000000000) even?) ;->#t
