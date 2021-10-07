#lang racket

(define (growing-plant upSpeed downSpeed desiredHeight)
    (define (helper upSpeed downSpeed desiredHeight currentHeight days)
        (cond
        [ (or (= desiredHeight upSpeed)(< desiredHeight upSpeed)) 1]
        [( = currentHeight desiredHeight) days]
        [else (helper upSpeed downSpeed desiredHeight (+ (- upSpeed downSpeed) currentHeight) (+ days 1))]
        )
    )
    (helper upSpeed downSpeed desiredHeight 0 0)
    
)

(= (growing-plant 5 2 5) 1)
(= (growing-plant 5 2 6) 2)
(= (growing-plant 10 9 4) 1)
(= (growing-plant 100 10 910) 10) ; upSpeed=100, downSpeed=10, desiredHeight=910