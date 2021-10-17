#lang racket
(require math/number-theory)

(define (sum-prime-divs-rec num)

(define (helper current-num)
    (cond
        [ (< num current-num) 0]
        [(and (prime? current-num) (divides? current-num num ))(+ (helper(+ 1 current-num)) current-num)]
        [else (helper (+ 1 current-num))]
    )
)   
 (helper 2)
)

(= (sum-prime-divs-rec 0) 0)
(= (sum-prime-divs-rec 6) 5) ; 2 + 3
(= (sum-prime-divs-rec 18) 5) ; 2 + 3
(= (sum-prime-divs-rec 19) 19)
(= (sum-prime-divs-rec 45136) 53)