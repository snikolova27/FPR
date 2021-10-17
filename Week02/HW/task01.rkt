#lang racket
(define (count-digits-iter num)
    (define (helper remaining digits)
      (if( <  remaining 10)
         (+ 1 digits)
         (helper (quotient remaining 10) (+ 1 digits))
         )
    )
    (cond
    [ (< num 0) (error "n was negative")]
    [(< num 10) 1]
    [else (helper num 0)]
    )
)



(define (count-digits-rec num)
  (cond
    [(< num 0) (error "n was negative")]
    [(< num 10) 1]
    [else (+ 1 (count-digits-rec (quotient num 10)))] 
  )
)

(= (count-digits-rec 12345) 5)
(= (count-digits-rec 123) 3)
;(count-digits-iter -13) ; error "n was negative"

(= (count-digits-iter 12345) 5)
(= (count-digits-iter 123) 3)
;(count-digits-iter -123)

