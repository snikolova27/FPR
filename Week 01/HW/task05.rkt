#lang racket

(define (snail  height slideUp slideDown)
    (cond
        [ (< slideUp slideDown) (error "Sliding up distance should be greater than sliding down distance")]
        [(<= height slideUp) 1]
        [(= (- slideUp slideDown) 1) (- height slideDown)]
        [ else (round(/ height (- slideUp slideDown)))]
    )
)

(= (snail 3 2 1) 2)
(= (snail 10 3 1) 5)
(= (snail 10 3 2) 8)
(= (snail 100 20 5) 7)
(= (snail 5 10 3) 1)
(= (snail 8 4 2 ) 4)
(= (snail 11 4 2 ) 6)
;(= (snail 5 1 4) 1) expected error