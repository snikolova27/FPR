#lang racket

(define (snail  height distance-day distance-night)
    (cond
        [(<= height distance-day) 1]
        [(= (- distance-day distance-night) 1) (- height distance-night)]
        [ else (round(/ height (- distance-day distance-night)))]
    )
)

(= (snail 3 2 1) 2)
(= (snail 10 3 1) 5)
(= (snail 10 3 2) 8)
(= (snail 100 20 5) 7)
(= (snail 5 10 3) 1)