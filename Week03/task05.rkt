#lang racket

(define (p n)
  (if (= 1 n)
      1
      (+ (* (sub1 n) n) (/ (* n (add1 n)) 2))
  )
)
  


(= (p 1) 1)
(= (p 2) 5)
(= (p 3) 12)
(= (p 4) 22)
(= (p 5) 35)
(= (p 6) 51)

;formula source https://www.nctm.org/News-and-Calendar/Messages-from-the-President/Archive/J_-Michael-Shaughnessy/Problems-to-Ponder/Problem-to-Ponder_-September-15,-2011/