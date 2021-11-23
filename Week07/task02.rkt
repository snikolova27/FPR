#lang racket
(require math/number-theory)
(require racket/trace)

(define (prime-divisors-cnt num)
  (define (helper cnt current-div current-num)
    (cond
      [(or (= 1 num) (= 2 num))1]
      [(>= current-div num) cnt]
      [(and (prime? current-div) (divides? current-div current-num)) (helper (add1 cnt) (add1 current-div )(/ current-num current-div))]
      [else (helper cnt (add1 current-div) current-num)]
    )
  )
 ;(trace helper)
  (helper 1 2 num)
)

(define (numbers n)
  (if (< n 1)
      (error "n must be natural")
       (lambda (k)
    (define (helper res current)
      (cond
        [(> current n) (reverse res)]
        [(>= k (prime-divisors-cnt current)) (helper (cons current res) (add1 current))]
        [else (helper res (add1 current))]
      )
    )
    ;(trace helper)
    (helper '() 1)
   )
      )
 
 )
;(prime-divisors-cnt 4)
;(prime-divisors-cnt 8)
;((numbers 10) 1)
(equal? ((numbers 10) 1) '(1 2 3 5 7))
(equal? ((numbers 20) 2) '(1 2 3 4 5 7 8 9 11 13 16 17 19))