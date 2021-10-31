#lang racket

(define (add-ones n)
  (define (helper left-over res pos)
    (cond
      [(zero? left-over) res]
      [(= (remainder left-over 10) 9) (helper (quotient left-over 10) (+ res (*(expt 10 pos) 0) (* 1( expt 10 (add1 pos)))) (+ 2 pos)) ]
      [else (helper (quotient left-over 10) (+ res (* (add1 (remainder left-over 10)) (expt 10 pos))) (add1 pos))]

    )
  )
  (helper n 0 0)
)

(add-ones 123) ; -> 234
(add-ones 193) ; -> 2104
(add-ones 998) ; -> 10109
(add-ones 9999) ; -> 10101010