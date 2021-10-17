#lang racket
(require math/number-theory) 

(define (max-multiple d b)
    (define (helper current )
        (cond
          [(zero? current) (error "No such number found")]
          [(divides? d current) current]
          [else (helper (sub1 current))]
        )
    )

    (helper b)

)

(= (max-multiple 2 7) 6)
(= (max-multiple 3 10) 9)
(= (max-multiple 7 17) 14)
(= (max-multiple 10 50) 50)
(= (max-multiple 37 200) 185)
(= (max-multiple 7 100) 98)