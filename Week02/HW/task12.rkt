#lang racket
(require racket/trace) 

(define (sum-n a b num)
  (define (helper current sum)
    (cond
      [(< num current) sum]
      [(zero? current) (helper (+ 1 current) (+ a  b sum)) ]
      [else (helper (+ 1 current) (+ sum (*(expt 2 current) b)))]
    )
  )
 ;(trace helper)
   (helper 0 0)
)

(define (find-sum a b n)
  (cond
    [(< n 3) (error "n will always be > 3")]
    [else (+ (sum-n a b (- n 3)) (sum-n a b (- n 2))  (sum-n a b (sub1 n)))]
    )
)
(= (find-sum 0 2 10) 3578) ; 510 + 1022 + 2046
(= (find-sum 5 3 5) 174) ; 26 + 50 + 98