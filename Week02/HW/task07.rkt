#lang racket

(define (count-occurences number digit)

    (define (helper remaining count)
        (cond
            [(<= remaining 0) count]
            [(= (remainder remaining 10) digit) (helper( quotient remaining 10) (+ 1 count))]
            [else (helper (quotient remaining 10) count)]
        )
    )
    (cond
        [(< number 0) (error "Negative number!")]
        [ (helper number 0)]
    )
)

(= (count-occurences 121 1) 2)
(= (count-occurences 14454 4) 3)
;(count-occurences -121 1) ; error "Negative number!"