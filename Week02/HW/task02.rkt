#lang racket

(define (sum-digits-iter num)
  (define (helper sum remaining)
    (if (< remaining 10)
        (+ sum (remainder remaining 10))
        (helper (+ (remainder remaining 10) sum) (quotient remaining 10))
    )
    )
  
     (cond
       [(< num 0) (error "n was negative")]
       [(< num 10) num]
       [else (helper 0 num)]
     )
)

(= (sum-digits-iter 12345) 15)
(= (sum-digits-iter 123) 6)
; (sum-digits-iter -13) ; error "n was negative"
