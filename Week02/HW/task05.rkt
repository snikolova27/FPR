#lang racket

(require math/number-theory)

;sum of divisors without the number itself
(define (sum-divs num)
  (define (helper sum current)
    (cond
      [(= current 1) (+ sum 1)  ]
      [(divides? current num) (helper (+ sum current)(sub1 current))]
      [else (helper sum (sub1 current))]
      )

    )
  (if (< num 1)
      0
      (helper 0 (sub1 num))
   )
)

(define (amicable? num-1 num-2)
     (=(sum-divs num-1)  num-2) 
)

(equal? (amicable? 200 300) #f)
(equal? (amicable? 220 284) #t)
(equal? (amicable? 284 220) #t)
(equal? (amicable? 1184 1210) #t)
(equal? (amicable? 2620 2924) #t)
(equal? (amicable? 6232 6368) #t)
