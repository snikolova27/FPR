#lang racket
(require math/number-theory) 

(define (sum-digits-iter num)
  (define (helper sum remaining)
    (if (< remaining 10)
        (+ sum (remainder remaining 10))
        (helper (+ (remainder remaining 10) sum) (quotient remaining 10))
    )
    )
  
     (cond
      ; [(< num 0) (error "n was negative")]
       [(< num 10) num]
       [else (helper 0 num)]
     )
)

(define (interesting? num)
( prime? (sum-digits-iter num))
)

(equal? (interesting? 410) #t)