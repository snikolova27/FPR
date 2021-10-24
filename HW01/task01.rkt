#lang racket
(require racket/trace)


;Task01

(define (digit-occurs? number digit)
    (cond
    [(= number digit) #t]
    [(= (remainder number 10) digit) #t]
    [(and(< number 10) (not (= number digit))) #f]
    [else   (digit-occurs? (quotient number 10) digit)]

 )
)

(define (times-occur number digit)
  (define (helper left-over cnt)
    (cond
      [(zero? left-over) cnt]
      [(= (remainder left-over 10) digit) (helper (quotient left-over 10) (add1 cnt))]
      [else (helper (quotient left-over 10) cnt)]
    )
  )
  (helper number 0)
)

 (define (sum-digits number)
   (define (helper left-over sum)
     (cond
       [(< left-over 10) (+ sum left-over)]
       [else (helper (quotient left-over 10) (+ sum (remainder left-over 10)))]
     )
   )
   (helper number 0)
 )

(define (sum-counts-iter x d)
  (define (helper current-number result)
    (cond
      [(> current-number x) result]
      [(digit-occurs? current-number d)
     (helper (add1 current-number) ( + (times-occur current-number d) result))]
      [else (helper (add1 current-number) result)]
    )
  )
  (if (<= x 0)
      (error "x should be a natural number")
  (sum-digits (helper 1 0))
   )
)
;(sum-counts-iter 24 2);
;(sum-counts-iter -5 4);
(sum-counts-iter 1 1) ; -> 1
(sum-counts-iter 5123 1) ; -> 19
(sum-counts-iter 1234 8) ; -> 10
(sum-counts-iter 5555 5) ; -> 10
(sum-counts-iter 65432 6) ; -> 11
(sum-counts-iter 70000 1) ; -> 11
(sum-counts-iter 123321 1) ; -> 29



;Task02


(define (add-ones n)
  (define (helper left-over res pos)
    (cond
      [(zero? left-over) res]
      [(= (remainder left-over 10) 9) (helper (quotient left-over 10) (+ res (*(expt 10 pos) 0) (* 1( expt 10 (add1 pos)))) (+ 2 pos)) ]
      [else (helper (quotient left-over 10) (+ res (* (add1 (remainder left-over 10)) (expt 10 pos))) (add1 pos))]

    )
  )
  (helper n 0 0)
)

(add-ones 123) ; -> 234
(add-ones 193) ; -> 2104
(add-ones 998) ; -> 10109
(add-ones 9999) ; -> 10101010
