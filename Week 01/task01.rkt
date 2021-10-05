#lang racket

(define (my-min-built-in-p x y)
  (min x y)
 )

(= (my-min-built-in-p 5 6) 5)


(define (my-min-if x y)
  (if (< x y)
      x
      y
      )
  )
(= (my-min-if -60 -15) -60)
(= (my-min-if 15 60) 15)
(= (my-min-if 60 15) 15)


(define (my-min-guard x y)
        (cond
          [(< x y) x] 
          [ else y ]
)
)

(= (my-min-guard 15 60) 15)
(= (my-min-guard 60 15) 15)


(define (last-digit x )
  (remainder x 10)
  )

(= (last-digit 154) 4)


(define (quotient-whole x y)
  (quotient x y)
  )

(= (quotient-whole 64 2) 32)


(define (div-whole x y)
  (/ x y)
  )

(div-whole 154 17)
; 9 1/17


(define (remove-last-digit x)
  (quotient x 10)
  )
(= (remove-last-digit 154) 15)


(define (div-real x y)
  (/ x y)
  )
(= (div-real 154 10.0) 15.4)

(define (round-two-dig x)
  ( / (round (* x 100)) 100)
  )
(= (round-two-dig pi) 3.14)

(define (average-whole x y)
  ( / (+ x y) 2.0 )
  )
(= (average-whole 5 1542) 773.5)