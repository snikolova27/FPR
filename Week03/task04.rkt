#lang racket
(require math/number-theory)

(define (sum-digits n)
  (define (helper left-over result)
   (if (< left-over 10)
       ( + result left-over)
       (helper (quotient left-over 10) (+ result (remainder left-over 10)))
   )
  )
  (helper n 0)
)
(define (sum-divisible-numbers start finish k)
  
  (define (helper sum current-num end)
    (cond
      [(> current-num end) sum]
      [(divides? k (sum-digits current-num))  (helper ( + sum current-num) (add1 current-num) end)]
      [else  (helper sum (add1 current-num) end)]
    )
    
  )
  (cond
    [(> start finish) (helper 0 finish start)]
    [(= start finish) (helper 0 start start)]
    [else (helper 0 start finish)]
   )
)


(= (sum-divisible-numbers 0 10 5) 5)
(= (sum-divisible-numbers 0 100 5) 990)
(= (sum-divisible-numbers 100 0 5) 990)