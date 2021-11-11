#lang racket
(require math/number-theory)

(define (prime-dividers n)
  (define (helper result left-over current-div)
    (cond
      [(= left-over 1) result]
      [(= current-div n) (cons current-div result)]
      [(and (prime? current-div) (divides? current-div left-over))
       (helper (cons current-div result) (/ left-over current-div) current-div )]
      [else  (helper result  left-over  (add1 current-div )) ]
    )
  )
  (helper '() n 2)
)

(define (factorize n)
  (cond
    [(> 1 n) (error "n should be a natural number")]
    [(> 2 n) (error "cannot factorize a number lesser than 2")]
    [else (sort (prime-dividers n) <=) ]
  )
)
(equal? (factorize 2) '(2))
(equal? (factorize 6) '(2 3))
(equal? (factorize 13) '(13))
(equal? (factorize 123) '(3 41))
(equal? (factorize 152) '(2 2 2 19))
(equal? (factorize 12356498) '(2 7 11 19 41 103))