#lang racket

(define (growing-plant upSpeed downSpeed desiredHeight)
  (cond
    [(> downSpeed upSpeed) (error "downSpeed is greater than upSpeed")]
    [(<= desiredHeight upSpeed) 1]
    [ else (round(/ desiredHeight (- upSpeed downSpeed)))]
    )
)
(= (growing-plant 5 2 5) 1)
(= (growing-plant 5 2 6) 2)
(= (growing-plant 10 9 4) 1)
(= (growing-plant 100 10 910) 10) ; upSpeed=100, downSpeed=10, desiredHeight=910
