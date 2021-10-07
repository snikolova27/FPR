#lang racket

(define (rev number)
  (cond
    [(< number 0) (error "Number cannot be negative")]
    [else
     (define (helper result remaining)
       (if (zero? remaining)
            result
            (helper (+ (* result 10) (remainder remaining 10)) (quotient remaining 10))
       )
      )
     (helper(remainder number 10) (quotient number 10))
    ]
  )
)

(= (rev 1) 1)
(= (rev 123) 321)
(= (rev 987654321) 123456789)
;(= (rev -2) 0) expected error