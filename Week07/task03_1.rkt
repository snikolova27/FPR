#lang racket
(require racket/trace)

(define (cnt-digits num)
  (define (helper cnt left-over)
    (cond
      [(< left-over 10) (add1 cnt)]
      [else (helper (add1 cnt) (quotient left-over 10))]
     )
   )
  (helper 0 num)
)

(define (reverse-num n)
  (define (helper new left-over current)
    (if (zero? left-over)
        new
        (helper (+ (* (remainder left-over 10) (expt 10 current) ) new) (quotient left-over 10) (sub1 current)))
  )
  ;(trace helper)
  (helper 0 n (sub1  (cnt-digits n)))
)

(define (pow-num-sum n p)
   (define (helper sum left-over current-p)
    (cond
      [(zero? left-over) sum]
      [else (helper (+ sum (expt (remainder left-over 10) current-p)) (quotient left-over 10) (add1 current-p))] 
    )
  )
  (helper 0 (reverse-num n) p)
)

(define (dig-pow n p)
 (define (helper current-k num-to-reach)
   (cond
     [(> current-k (sqrt num-to-reach)) -1]
     [( = (* current-k n) num-to-reach) current-k]
     [else (helper (add1 current-k) num-to-reach)]
   )
 )
  (helper 1 (pow-num-sum n p))
)

;(reverse-num 695)
;(pow-num-sum 695 2)

(= (dig-pow 89 1) 1)
(= (dig-pow 92 1) -1)
(= (dig-pow 695 2) 2)
(= (dig-pow 46288 3) 51)
