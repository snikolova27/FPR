#lang racket

(require racket/trace)

(define (in-descending? num)
  (define (helper1 left-over)
    (cond
      [(< left-over 10) #t]
      [(>= (remainder (quotient left-over 10) 10)(remainder left-over 10)) (helper1 (quotient left-over 10))]
      [else #f]
    )
  )
 ;(trace helper1)
  (helper1 num)
)

(define (sum-numbers a b)
  (define (helper result current-num)
    (cond
      [(> current-num b) result]
      [ (in-descending? current-num) (helper (+ result current-num) (add1 current-num))]
      [else (helper result (add1 current-num))]
    )
  )
  (trace helper)
  (helper 0 a)
)

(equal? (sum-numbers 1 9) 45)
(equal? (sum-numbers 199 203) 200)
(equal? (sum-numbers 219 225) 663)
(= (sum-numbers 4 25) 123)