#lang racket
(require math/number-theory)

(define (contains-digit? num digit)
  (define (helper remaining)
    (cond
      [(zero? remaining) #f]
      [(and (< remaining 10) (= remaining digit)) #t]
      [(= digit (remainder remaining 10)) #t]
      [else (helper (quotient remaining 10))] 
    )
  )
  
  (helper num)
)

(define (sum-special-primes n d)
    (define (helper current-num nums-left sum)
        (cond
        [(zero? nums-left) sum]
        [(and (prime? current-num) (contains-digit? current-num d)) (helper (+ 1 current-num) (sub1 nums-left) (+ current-num sum))] 
        [else (helper (+ 1 current-num) nums-left sum)]
        )
    )
    (helper 2 n 0)
)

(= (sum-special-primes 5 2) 392)
(= (sum-special-primes 5 3) 107)
(= (sum-special-primes 10 3) 462)