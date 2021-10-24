#lang racket

(require math/number-theory)
 (require racket/trace)

(define (denominator-n n)
  ( define (helper current-n result multiplier)
          (if (= current-n n)
              result
              (helper (add1 current-n) (* result multiplier) (+ 2 multiplier ))
          ) 
  )
  (helper 0 1 1)
)


(define (calc-series-sum x n)
 (define (helper current-n result)
   (if ( > current-n n)
       result
       (helper (add1 current-n) (+ result (/ (* (expt -1 (add1 current-n)) (expt  2 (add1 current-n))  (expt x current-n))
                                             (denominator-n (add1 current-n)))))
   )
  
 )
  (helper 0 0)
)

(calc-series-sum 1 0) ; -2 ;not properly
(calc-series-sum 1 1) ; -2/3
(calc-series-sum 1 2) ; -1 1/5
(calc-series-sum 1 3) ; -1 1/21
(calc-series-sum 1 4) ; -1 11/135
(calc-series-sum 1 5) ; -1 29/385
(calc-series-sum 1 6) ; -1 937/12285