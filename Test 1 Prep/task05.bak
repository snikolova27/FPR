#lang racket
(require math/number-theory)

(define (sum-digits num)
 (define (helper result left-over)
   (cond
     [(< left-over 10) (+ result left-over)]
     [else (helper (+ result (remainder left-over 10)) (quotient left-over 10))]
   )
 )
  (helper 0 num)
)

(define (sum-sum-digit a b k)
  (define (helper result current)
    (cond
      [(> current b) result]
      [(divides? k (sum-digits current)) (helper (+ result current) (add1 current))]
      [else (helper result (add1 current))] 
    )
  )
  
  (cond
    [(< a 0) (error "a should be natural")]
    [(< b 0 ) (error "b should be natural")]
    [else (helper 0 a)]
  )
)

(= (sum-sum-digit 2 14 2) 44)